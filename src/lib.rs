use std::fmt::Display;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::ToTokens;

use syn::Arm;
use syn::Expr;
use syn::ExprMatch;
use syn::spanned::Spanned;

use syn::PatIdent;
use syn::Stmt;

use syn::{ItemFn, Result, visit_mut::VisitMut};

use syn::{Error, Item, parse_macro_input};

struct MyPath<'a>(&'a syn::Path);
impl<'a> Display for MyPath<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let vec = &self
            .0
            .segments
            .iter()
            .map(|path_segment| path_segment.ident.to_string())
            .collect::<Vec<_>>();
        let path_string = vec[..].join(&String::from("::"));
        write!(f, "{path_string}")
    }
}

pub(crate) fn expand_item_fn(mut item_fn: ItemFn) -> Result<proc_macro2::TokenStream> {
    let block = item_fn.block.as_mut();
    let stmts = &mut block.stmts;
    for stmt in stmts {
        if let Stmt::Expr(Expr::Match(expr_match), _) = stmt {
            let arms = &expr_match.arms;

            let attrs = &expr_match.attrs;
            let mut sorted_flag = false;
            for attr in attrs {
                if let syn::Meta::Path(s) = &attr.meta {
                    let a = s.get_ident().unwrap();
                    if a == "sorted" {
                        sorted_flag = true
                    }
                }
            }

            if sorted_flag {
                let mut vistor = Visitor;
                let slice = &mut arms.iter().collect::<Vec<_>>()[..];

                for (index, _) in arms.iter().enumerate() {
                    let (left, right) = slice.split_at_mut(index);

                    let compared_object = left.last();
                    if compared_object.is_none() {
                        continue;
                    }

                    for x in right {
                        let (compared_object_path_string, _span, compared_object_wild_value) =
                            get_ident(compared_object.unwrap())?;
                        let (x_path_string, x_span, _x_wild_value) = get_ident(x)?;

                        if compared_object_wild_value || x_path_string < compared_object_path_string
                        {
                            let error = Err(syn::Error::new(
                                x_span,
                                format!(
                                    "{} should sort before {}",
                                    x_path_string, compared_object_path_string
                                ),
                            ));
                            vistor.visit_expr_match_mut(expr_match);

                            return error;
                        }
                    }
                }
                vistor.visit_expr_match_mut(expr_match);
            }
        }
    }
    return Ok(item_fn.to_token_stream());
}

#[proc_macro_attribute]
pub fn check(_: TokenStream, input: TokenStream) -> TokenStream {
    let input_item = parse_macro_input!(input as ItemFn);
    let result = expand_item_fn(input_item).unwrap_or_else(Error::into_compile_error);
    result.into()
}

fn get_ident(arm: &Arm) -> syn::Result<(String, Span, bool)> {
    // the 3rd one is wild value flag
    let mut wild_value = false;
    // fn get_ident(arm: &Arm) -> TokenStream {
    match &arm.pat {
        syn::Pat::Path(syn::ExprPath { path, .. })
        | syn::Pat::Struct(syn::PatStruct { path, .. })
        | syn::Pat::TupleStruct(syn::PatTupleStruct { path, .. }) => {
            let span = path.span();

            return Ok((MyPath(path).to_string(), span, wild_value));
        }
        syn::Pat::Ident(PatIdent { ident, .. }) => {
            let span = ident.span();

            return Ok((ident.to_string(), span, wild_value));
        }
        syn::Pat::Wild(path_wild) => {
            let span = path_wild.span();
            wild_value = true;
            return Ok(("_".to_string(), span, wild_value));
        }
        other_pat => {
            let span = other_pat.span();
            return Err(syn::Error::new(span, "unsupported by #[sorted]"));
        }
    }
}

struct Visitor;
impl VisitMut for Visitor {
    fn visit_expr_match_mut(&mut self, expr_match: &mut ExprMatch) {
        expr_match.attrs = vec![];
    }
}

#[proc_macro_attribute]
pub fn sorted(_: TokenStream, input: TokenStream) -> TokenStream {
    let input_item = parse_macro_input!(input as Item);
    let result = expand(input_item).unwrap_or_else(Error::into_compile_error);
    result.into()
}

pub(crate) fn expand(input_item: Item) -> Result<proc_macro2::TokenStream> {
    if let Item::Enum(s) = input_item {
        let variants = &s.variants;
        let slice = &mut variants.iter().collect::<Vec<_>>()[..];
        for (index, _) in variants.iter().enumerate() {
            let (left, right) = slice.split_at_mut(index);
            let compared_object = left.last();
            if compared_object.is_none() {
                continue;
            }
            for x in right {
                if x.ident <= compared_object.unwrap().ident {
                    let error = Err(syn::Error::new(
                        x.ident.span(),
                        // Span::call_site(),
                        format!(
                            "{} should sort before {}",
                            x.ident,
                            compared_object.unwrap().ident
                        ),
                    ));
                    return error;
                }
            }
        }

        Ok(s.to_token_stream())
    } else {
        return Err(syn::Error::new(
            // input.span(),
            Span::call_site(),
            "expected enum or match expression",
        ));
    }
}
