use std::path::Path;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::ToTokens;

use syn::Arm;
use syn::Expr;
use syn::ExprMatch;

use syn::Stmt;

use syn::{ItemFn, Result, visit_mut::VisitMut};

use syn::{Error, Item, parse_macro_input};

pub(crate) fn expand_item_fn(mut item_fn: ItemFn) -> Result<proc_macro2::TokenStream> {
    // eprintln!("item_fn's in check is {:#?}", item_fn);
    let block = item_fn.block.as_mut();
    let stmts = &mut block.stmts;
    for stmt in stmts {
        if let Stmt::Expr(Expr::Match(expr_match), _) = stmt {
            let arms = &expr_match.arms;
            let attrs = &expr_match.attrs;
            for attr in attrs {
                if let syn::Meta::Path(s) = &attr.meta {
                    let a = s.get_ident().unwrap();
                    if a == "sorted" {
                        let slice = &mut arms.iter().collect::<Vec<_>>()[..];

                        for (index, _) in arms.iter().enumerate() {
                            let (left, right) = slice.split_at_mut(index);
                            let compared_object = left.last();
                            if compared_object.is_none() {
                                continue;
                            }
                            for x in right {
                                let x_ident: &syn::Ident = get_ident(x).unwrap();
                                let compared_object_ident: &syn::Ident =
                                    get_ident(compared_object.unwrap()).unwrap();
                                if x_ident <= compared_object_ident {
                                    let error = Err(syn::Error::new(
                                        x_ident.span(),
                                        // Span::call_site(),
                                        format!(
                                            "{} should sort before {}",
                                            x_ident, compared_object_ident
                                        ),
                                    ));
                                    let mut vistor = Visitor;
                                    vistor.visit_expr_match_mut(expr_match);

                                    return error;
                                }
                            }
                        }
                    }
                }
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

    // let result = expand(input_item).unwrap_or_else(Error::into_compile_error);
    // result.into()
}

fn get_ident(arm: &Arm) -> Option<&syn::Ident> {
    if let syn::Pat::TupleStruct(syn::PatTupleStruct {
        path: syn::Path { segments, .. },
        ..
    }) = &arm.pat
    {
        return Some(&segments[0].ident);
    }
    return None;
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
