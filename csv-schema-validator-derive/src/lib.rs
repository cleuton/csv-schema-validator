// csv-schema-validator-derive/src/lib.rs
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Expr, ExprLit, Fields, GenericArgument, Ident, Lit, Meta,
    PathArguments, Type,
};

// Armazena validações por campo
struct FieldValidation {
    field_name: Ident,
    is_option: bool, // [FIX] passamos a carregar se o campo é Option<T>
    validations: Vec<Validation>,
}

// Tipos de validações suportadas
enum Validation {
    Range { min: f64, max: f64 },
    Regex { regex: String },
    Required,
    Custom { path: syn::Path },
    Length { min: usize, max: usize },
    NotBlank,
    OneOf { values: Vec<String> },
    NotIn { values: Vec<String> },    
}

impl Validation {
    /// Faz o parse de #[validate(...)] em uma lista de Validation
    fn parse_validations(input: syn::parse::ParseStream) -> syn::Result<Vec<Self>> {
        let mut out = Vec::new();
        let meta_items = syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated(input)?;
        for meta in meta_items {
            match meta {
                syn::Meta::Path(path) => Self::parse_meta_path(path, &mut out)?,
                syn::Meta::NameValue(mnv) => Self::parse_meta_name_value(mnv, &mut out)?,
                syn::Meta::List(list) => Self::parse_meta_list(list, &mut out)?,
            }
        }
        Ok(out)
    }

    // ---------- Dispatchers por variante de Meta ----------

    fn parse_meta_path(path: syn::Path, out: &mut Vec<Self>) -> syn::Result<()> {
        if path.is_ident("required") {
            out.push(Validation::Required);
        } else if path.is_ident("not_blank") {
            out.push(Validation::NotBlank);
        }
        Ok(())
    }

    fn parse_meta_name_value(mnv: syn::MetaNameValue, out: &mut Vec<Self>) -> syn::Result<()> {
        if mnv.path.is_ident("regex") {
            let s = Self::expect_lit_str(&mnv.value, "Expected string literal for `regex`")?;
            out.push(Validation::Regex { regex: s });
        } else if mnv.path.is_ident("custom") {
            let s = Self::expect_lit_str(&mnv.value, "Expected string literal for `custom` (e.g., custom = \"path::to::fn\")")?;
            let path: syn::Path = syn::parse_str(&s).map_err(|e| syn::Error::new_spanned(&mnv.value, e))?;
            out.push(Validation::Custom { path });
        }
        Ok(())
    }

    fn parse_meta_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let ident = &list.path;
        if ident.is_ident("length") {
            Self::parse_length_list(list, out)
        } else if ident.is_ident("range") {
            Self::parse_range_list(list, out)
        } else if ident.is_ident("one_of") {
            Self::parse_one_of_list(list, out)
        } else if ident.is_ident("not_in") {
            Self::parse_not_in_list(list, out)
        } else {
            Ok(())
        }
    }

    // ---------- Handlers específicos ----------

    fn parse_length_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let items: syn::punctuated::Punctuated<syn::MetaNameValue, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut min: Option<usize> = None;
        let mut max: Option<usize> = None;

        for kv in items {
            if kv.path.is_ident("min") {
                let v = Self::expect_lit_int(&kv.value, "`min` for `length` must be an integer literal")?;
                min = Some(v);
            } else if kv.path.is_ident("max") {
                let v = Self::expect_lit_int(&kv.value, "`max` for `length` must be an integer literal")?;
                max = Some(v);
            }
        }

        if min.is_none() && max.is_none() {
            return Err(syn::Error::new_spanned(list, "`length` requires at least one of `min` or `max`"));
        }
        if let Some(mx) = max {
            if mx == 0 {
                return Err(syn::Error::new_spanned(list, "`max` for `length` cannot be zero"));
            }
        }
        if let (Some(a), Some(b)) = (min, max) {
            if a > b {
                return Err(syn::Error::new_spanned(list, "`min` must be <= `max` for `length`"));
            }
        }

        out.push(Validation::Length {
            min: min.unwrap_or(0),
            max: max.unwrap_or(usize::MAX),
        });
        Ok(())
    }

    fn parse_range_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let items: syn::punctuated::Punctuated<syn::MetaNameValue, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut min: Option<f64> = None;
        let mut max: Option<f64> = None;

        for kv in items {
            if kv.path.is_ident("min") {
                let v = Self::expect_lit_float(&kv.value, "`min` for `range` must be a float literal")?;
                min = Some(v);
            } else if kv.path.is_ident("max") {
                let v = Self::expect_lit_float(&kv.value, "`max` for `range` must be a float literal")?;
                max = Some(v);
            }
        }

        if min.is_none() && max.is_none() {
            return Err(syn::Error::new_spanned(list, "`range` requires at least one of `min` or `max`"));
        }

        out.push(Validation::Range {
            min: min.unwrap_or(f64::NEG_INFINITY),
            max: max.unwrap_or(f64::INFINITY),
        });
        Ok(())
    }

    fn parse_one_of_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let exprs: syn::punctuated::Punctuated<syn::Expr, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut values = Vec::new();
        for expr in exprs {
            let s = Self::expect_lit_str_expr(expr, "`one_of` only accepts string literals")?;
            values.push(s);
        }
        if values.is_empty() {
            return Err(syn::Error::new_spanned(list, "`one_of` requires at least one value"));
        }
        out.push(Validation::OneOf { values });
        Ok(())
    }

    fn parse_not_in_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let exprs: syn::punctuated::Punctuated<syn::Expr, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut values = Vec::new();
        for expr in exprs {
            let s = Self::expect_lit_str_expr(expr, "`not_in` only accepts string literals")?;
            values.push(s);
        }
        if values.is_empty() {
            return Err(syn::Error::new_spanned(list, "`not_in` requires at least one value"));
        }
        out.push(Validation::NotIn { values });
        Ok(())
    }

    // ---------- Utilitários de extração ----------

    fn expect_lit_str(expr: &syn::Expr, msg: &str) -> syn::Result<String> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = expr {
            Ok(s.value())
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }

    fn expect_lit_int(expr: &syn::Expr, msg: &str) -> syn::Result<usize> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(i), .. }) = expr {
            i.base10_parse::<usize>().map_err(|e| syn::Error::new_spanned(expr, e))
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }

    fn expect_lit_float(expr: &syn::Expr, msg: &str) -> syn::Result<f64> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Float(f), .. }) = expr {
            f.base10_parse::<f64>().map_err(|e| syn::Error::new_spanned(expr, e))
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }

    fn expect_lit_str_expr(expr: syn::Expr, msg: &str) -> syn::Result<String> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = expr {
            Ok(s.value())
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }
}


// [FIX] helper para detectar Option<T>
fn option_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            if seg.ident == "Option" {
                if let PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

#[proc_macro_derive(ValidateCsv, attributes(validate))]
pub fn validate_csv_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(f) => &f.named,
            _ => {
                return syn::Error::new_spanned(
                    &data.fields,
                    "only structs with named fields are supported",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return syn::Error::new_spanned(&input, "only structs are supported")
                .to_compile_error()
                .into();
        }
    };

    let mut field_validations = Vec::new();

    for field in fields {
        let field_name = field.ident.as_ref().unwrap().clone();
        let is_option = option_inner_type(&field.ty).is_some(); // [FIX] capturamos se é Option<T>
        let mut validations = Vec::new();

        for attr in &field.attrs {
            if attr.path().is_ident("validate") {
                match attr.parse_args_with(Validation::parse_validations) {
                    Ok(mut v) => validations.append(&mut v),
                    Err(e) => return e.to_compile_error().into(),
                }
            }
        }

        if !validations.is_empty() {
            field_validations.push(FieldValidation {
                field_name,
                is_option,
                validations,
            });
        }
    }

    let validation_arms = field_validations.into_iter().map(|fv| {
        let field_name_str = fv.field_name.to_string();
        let field_name_ident = fv.field_name;
        let fv_is_option = fv.is_option;
    
        let checks = fv.validations.into_iter().map(|validation| {
            match validation {
                Validation::Required => {
                    gen_required_check(&field_name_ident, &field_name_str)
                }
                Validation::NotBlank => {
                    gen_not_blank_check(&field_name_ident, &field_name_str, fv_is_option)
                }
                Validation::Range { min, max } => {
                    gen_range_check(&field_name_ident, &field_name_str, fv_is_option, min, max)
                }
                Validation::Length { min, max } => {
                    gen_length_check(&field_name_ident, &field_name_str, fv_is_option, min, max)
                }
                Validation::Regex { regex } => {
                    gen_regex_check(&field_name_ident, &field_name_str, fv_is_option, regex)
                }
                Validation::OneOf { values } => {
                    gen_one_of_check(&field_name_ident, &field_name_str, fv_is_option, values)
                }
                Validation::NotIn { values } => {
                    gen_not_in_check(&field_name_ident, &field_name_str, fv_is_option, values)
                }
                Validation::Custom { path } => {
                    gen_custom_check(&field_name_ident, &field_name_str, fv_is_option, path)
                }
            }
        });
    
        quote! { #(#checks)* }
    });

    let expanded = quote! {
        impl #name {
            pub fn validate_csv(&self) -> ::core::result::Result<(), ::std::vec::Vec<::csv_schema_validator::ValidationError>> {
                let mut errors = ::std::vec::Vec::new();
                #(#validation_arms)*
                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(errors)
                }
            }
        }
    };

    TokenStream::from(expanded)
}

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

// Helpers por regra
fn gen_required_check(field_ident: &syn::Ident, field_name: &str) -> TokenStream2 {
    quote! {
        if (&self.#field_ident).is_none() {
            errors.push(::csv_schema_validator::ValidationError {
                field: #field_name.to_string(),
                message: "mandatory field".to_string(),
            });
        }
    }
}

fn gen_not_blank_check(field_ident: &syn::Ident, field_name: &str, is_option: bool) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                if value.trim().is_empty() {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: "must not be blank or contain only whitespace".to_string(),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            if value.trim().is_empty() {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: "must not be blank or contain only whitespace".to_string(),
                });
            }
        }
    }
}

fn gen_range_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, min: f64, max: f64) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                if !(#min <= *value && *value <= #max) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("value out of expected range: {} to {}", #min, #max),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            if !(#min <= *value && *value <= #max) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("value out of expected range: {} to {}", #min, #max),
                });
            }
        }
    }
}

fn gen_length_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, min: usize, max: usize) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                let len = value.len();
                if len < #min || len > #max {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("length out of expected range: {} to {}", #min, #max),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            let len = value.len();
            if len < #min || len > #max {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("length out of expected range: {} to {}", #min, #max),
                });
            }
        }
    }
}

fn gen_regex_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, regex: String) -> TokenStream2 {
    let body = quote! {
        use ::csv_schema_validator::__private::once_cell::sync::Lazy;
        use ::csv_schema_validator::__private::regex;
        static RE: Lazy<Result<regex::Regex, regex::Error>> = Lazy::new(|| regex::Regex::new(#regex));

        match RE.as_ref() {
            Ok(compiled_regex) => {
                if !compiled_regex.is_match(value) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: "does not match the expected pattern".to_string(),
                    });
                }
            }
            Err(e) => {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("invalid regex '{}': {}", #regex, e),
                });
            }
        }
    };
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                #body
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            #body
        }
    }
}

fn gen_one_of_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, values: Vec<String>) -> TokenStream2 {
    let arr = values; // mantém o padrão já usado
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                const __ALLOWED: &[&str] = &[#(#arr),*];
                if !__ALLOWED.contains(&value.as_str()) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("invalid value"),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            const __ALLOWED: &[&str] = &[#(#arr),*];
            if !__ALLOWED.contains(&value.as_str()) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("invalid value"),
                });
            }
        }
    }
}

fn gen_not_in_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, values: Vec<String>) -> TokenStream2 {
    let arr = values;
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                const __FORBIDDEN: &[&str] = &[#(#arr),*];
                if __FORBIDDEN.contains(&value.as_str()) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("value not allowed"),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            const __FORBIDDEN: &[&str] = &[#(#arr),*];
            if __FORBIDDEN.contains(&value.as_str()) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("value not allowed"),
                });
            }
        }
    }
}

fn gen_custom_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, path: syn::Path) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                match #path(value) {
                    Err(err) => {
                        errors.push(::csv_schema_validator::ValidationError {
                            field: #field_name.to_string(),
                            message: format!("{}", err),
                        });
                    }
                    Ok(()) => {}
                }
            }
        }
    } else {
        quote! {
            match #path(&self.#field_ident) {
                Err(err) => {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("{}", err),
                    });
                }
                Ok(()) => {}
            }
        }
    }
}
