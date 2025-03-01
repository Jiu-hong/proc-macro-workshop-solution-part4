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
        Ok(s.to_token_stream())
    } else {
        return Err(syn::Error::new(
            // input.span(),
            Span::call_site(),
            "expected enum or match expression",
        ));
    }
}
