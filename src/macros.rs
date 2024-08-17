#[macro_export]
macro_rules! lazy_mkv {
    ([$($strings:literal),*$(,)?]) => {{
        static MKV: ::std::sync::LazyLock<$crate::MarkovData> = ::std::sync::LazyLock::new(|| {
            $crate::MarkovData::from_strings(&[$($strings),*])
        });
        &*MKV
    }};
}
