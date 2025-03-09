use std::fmt::Display;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::ToTokens;

use syn::{
    Arm, Error, Expr, ExprMatch, Item, ItemFn, PatIdent, Result, Stmt, parse_macro_input,
    spanned::Spanned, visit_mut::VisitMut,
};

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

pub(crate) fn expand_expr_match(expr_match: &ExprMatch) -> Result<proc_macro2::TokenStream> {
    let arms = &expr_match.arms;

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

            if compared_object_wild_value || x_path_string < compared_object_path_string {
                let error = Err(syn::Error::new(
                    x_span,
                    format!(
                        "{} should sort before {}",
                        x_path_string, compared_object_path_string
                    ),
                ));

                return error;
            }
        }
    }

    return Ok(expr_match.to_token_stream());
}

#[proc_macro_attribute]
pub fn check(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut input_item = parse_macro_input!(input as ItemFn);

    // first remove the attribute here .
    let block = input_item.block.as_mut();
    let stmts = &mut block.stmts;
    for stmt in stmts {
        // eprintln!("stmt is {:#?}", stmt);
        if let Stmt::Expr(Expr::Match(expr_match), _) = stmt {
            let attrs = &expr_match.clone().attrs;
            for attr in attrs {
                if let syn::Meta::Path(s) = &attr.meta {
                    let a = s.get_ident().unwrap();
                    if a == "sorted" {
                        let mut vistor = Visitor;
                        vistor.visit_expr_match_mut(expr_match);
                        let pre_result = expand_expr_match(expr_match);
                        if pre_result.is_err() {
                            let error_token_stream = TokenStream::from(
                                pre_result.unwrap_or_else(Error::into_compile_error),
                            );
                            return TokenStream::from_iter(vec![
                                error_token_stream,
                                input_item.into_token_stream().into(),
                            ]);
                        }
                    }
                }
            }
        }
    }
    input_item.into_token_stream().into()
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
    let input_clone = input.clone();
    let input_item = parse_macro_input!(input as Item);
    // eprintln!("input_item is {:#?}", input_item);
    let pre_result = expand(input_item);
    // let result = expand(input_item).unwrap_or_else(Error::into_compile_error);
    if pre_result.is_err() {
        let error_token_stream =
            TokenStream::from(pre_result.unwrap_or_else(Error::into_compile_error));
        return TokenStream::from_iter(vec![error_token_stream, input_clone]);
    }
    pre_result.unwrap().into()
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
