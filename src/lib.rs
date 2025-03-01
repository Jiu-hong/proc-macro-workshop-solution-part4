use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::ToTokens;
use syn::Result;

use syn::{Error, Item, parse_macro_input};

#[proc_macro_attribute]
pub fn sorted(_: TokenStream, input: TokenStream) -> TokenStream {
    let input_item = parse_macro_input!(input as Item);
    expand(input_item)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

pub(crate) fn expand(input_item: Item) -> Result<proc_macro2::TokenStream> {
    // eprintln!("input is {:#?}", input_item);
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
                    println!("something wrong");
                    return Err(syn::Error::new(
                        x.ident.span(),
                        // Span::call_site(),
                        format!(
                            "{} should sort before {}",
                            x.ident,
                            compared_object.unwrap().ident
                        ),
                    ));
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
