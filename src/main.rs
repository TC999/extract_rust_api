use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use syn::{
    punctuated::Punctuated, token::Comma, FnArg, ImplItem, Item, File, Pat, PathArguments, ReturnType, Type,
};
use syn::spanned::Spanned;  // ← 导入 Span 支持
use walkdir::WalkDir;

#[derive(Debug, Serialize, Deserialize, Clone)]
struct RustFunction {
    pub name: String,
    pub signature: String,      // 例如 "fn foo(i32) -> bool"
    pub file: String,
    pub line: usize,
    pub kind: String,          // "free" 或 "method"
    pub parent: String,        // 如果是方法，记录结构体名
}

/// 将 `syn::Type` 转换成可读字符串（不展开泛型细节，仅用于显示）
fn ty_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(p) => {
            let path = &p.path;
            let ident = path.segments.last().map(|seg| seg.ident.to_string()).unwrap_or("_".into());
            // 尝试提取泛型参数（简单处理）
            let args: Vec<String> = path.segments.iter().flat_map(|seg| {
                match &seg.arguments {
                    PathArguments::AngleBracketed(ab) => {
                        ab.args.iter().filter_map(|arg| {
                            match arg {
                                syn::GenericArgument::Type(ty) => Some(ty_to_string(ty)),
                                _ => None,
                            }
                        }).collect::<Vec<_>>()
                    }
                    _ => vec![],
                }
            }).collect();
            if !args.is_empty() {
                format!("{}<{}>", ident, args.join(", "))
            } else {
                ident
            }
        }
        Type::Reference(r) => {
            let mut s = String::new();
            if r.mutability.is_some() {
                s.push_str("&mut ");
            } else {
                s.push_str("&");
            }
            s.push_str(&ty_to_string(&r.elem));
            s
        }
        Type::Ptr(p) => {
            let mut s = String::new();
            if p.mutability.is_some() {
                s.push_str("*mut ");
            } else {
                s.push_str("*const ");
            }
            s.push_str(&ty_to_string(&p.elem));
            s
        }
        Type::Tuple(t) => {
            let elems: Vec<String> = t.elems.iter().map(|ty| ty_to_string(ty)).collect();
            format!("({})", elems.join(", "))
        }
        Type::Array(a) => {
            let len = match &a.len {
                syn::Expr::Lit(lit) => lit_to_string(&lit.lit),
                _ => "_".into(),
            };
            format!("[{}; {}]", ty_to_string(&a.elem), len)
        }
        Type::Slice(s) => format!("[{}]", ty_to_string(&s.elem)),
        Type::Paren(p) => format!("({})", ty_to_string(&p.elem)),
        Type::Group(g) => ty_to_string(&g.elem),
        Type::ImplTrait(_) => "impl Trait".into(),
        Type::TraitObject(_) => "dyn Trait".into(),
        _ => "_".into(),
    }
}

/// 将 `syn::Lit` 转换成可读字符串（用于数组长度表达式）
fn lit_to_string(lit: &syn::Lit) -> String {
    match lit {
        syn::Lit::Int(i) => i.base10_digits().to_string(),
        syn::Lit::Float(f) => f.base10_digits().to_string(),
        syn::Lit::Str(s) => format!("\"{}\"", s.value()),
        syn::Lit::Char(c) => format!("'{}'", c.value()),
        syn::Lit::Byte(b) => format!("b'{}'", b.value() as char),
        syn::Lit::ByteStr(bs) => format!("b\"{:?}\"", bs.value()),
        syn::Lit::Bool(b) => b.value().to_string(),
        syn::Lit::Verbatim(v) => v.to_string(),
        _ => "_".into(),
    }
}

/// 构建函数签名的字符串表示（不包含函数名）
fn build_fn_signature(inputs: &Punctuated<FnArg, Comma>, output: &ReturnType) -> String {
    let params: Vec<String> = inputs.iter().map(|arg| {
        match arg {
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;
                let ty_str = ty_to_string(ty);
                // 尝试提取参数名（如果是 Pat::Ident）
                if let Pat::Ident(pat_ident) = &*pat_type.pat {
                    format!("{}: {}", pat_ident.ident, ty_str)
                } else {
                    ty_str
                }
            }
            FnArg::Receiver(recv) => {
                let mut s = "self".to_string();
                if recv.mutability.is_some() {
                    s.push_str(" mut");
                }
                if let Some(lt) = recv.lifetime() {
                    // lifetime() 返回 Option<&Lifetime>
                    s = format!("{} '{}", s, lt.ident);
                }
                s = format!("{}: {}", s, ty_to_string(&recv.ty));
                s
            }
        }
    }).collect();

    let ret = match output {
        ReturnType::Default => "".to_string(),
        ReturnType::Type(_, ty) => format!(" -> {}", ty_to_string(ty)),
    };
    format!("({}){}", params.join(", "), ret)
}

fn extract_from_file(path: &Path) -> Vec<RustFunction> {
    let content = fs::read_to_string(path).unwrap_or_default();
    let syntax: File = syn::parse_file(&content).unwrap_or_else(|_| panic!("Failed to parse {:?}", path));
    let mut results = Vec::new();

    for item in syntax.items {
        match item {
            Item::Fn(func) => {
                let sig = &func.sig;
                let name = sig.ident.to_string();
                let sig_str = build_fn_signature(&sig.inputs, &sig.output);
                let line = sig.span().start().line;
                results.push(RustFunction {
                    name: name.clone(),
                    signature: format!("fn {}{}", name, sig_str),
                    file: path.display().to_string(),
                    line,
                    kind: "free".to_string(),
                    parent: "".to_string(),
                });
            }
            Item::Impl(impl_block) => {
                // 获取实现类型名称（简化）
                let self_ty = match &*impl_block.self_ty {
                    Type::Path(p) => p.path.segments.last().map(|s| s.ident.to_string()).unwrap_or("_".into()),
                    _ => "_".into(),
                };
                for inner in impl_block.items {
                    if let ImplItem::Fn(method) = inner {
                        let sig = &method.sig;
                        let name = sig.ident.to_string();
                        let sig_str = build_fn_signature(&sig.inputs, &sig.output);
                        let line = sig.span().start().line;
                        results.push(RustFunction {
                            name: name.clone(),
                            signature: format!("fn {}{}", name, sig_str),
                            file: path.display().to_string(),
                            line,
                            kind: "method".to_string(),
                            parent: self_ty.clone(),
                        });
                    }
                }
            }
            _ => {}
        }
    }
    results
}

fn main() -> anyhow::Result<()> {
    let src_dir = "./src"; // 可根据项目调整，或通过命令行传入
    let mut all_funcs = Vec::new();

    for entry in WalkDir::new(src_dir).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            all_funcs.extend(extract_from_file(path));
        }
    }

    // 去重（按名称+父级）
    let mut seen = HashMap::new();
    let mut unique = Vec::new();
    for f in all_funcs {
        let key = if f.kind == "method" {
            format!("{}::{}", f.parent, f.name)
        } else {
            f.name.clone()
        };
        if !seen.contains_key(&key) {
            seen.insert(key, true);
            unique.push(f);
        }
    }

    let json = serde_json::to_string_pretty(&unique)?;
    fs::write("rust_functions_manifest.json", json)?;
    println!("Extracted {} unique Rust functions.", unique.len());
    Ok(())
}