use wasmcraft::RunOptions;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    let run_options = parse_args(args.into_iter()).unwrap();

    wasmcraft::run(&run_options);
}

enum ParseErr {

}

fn parse_args<I>(mut args: I) -> Result<RunOptions, String>
    where I: Iterator<Item=String>
{
    let mut wasm_path = None;
    let mut out_path = None;

    let mut parse_mode = "";

    for arg in args {
        if parse_mode.is_empty() {
            if wasm_path.is_some() {
                return Err("more than one input path provided".to_string());
            } else if arg == "-o" {
                parse_mode = "-o";
            } else {
                wasm_path = Some(arg.into());
            }
        } else if parse_mode == "-o" {
            if out_path.is_some() {
                return Err("more than one output path provided".to_string());
            } else {
                out_path = Some(arg.into());
            }
            parse_mode = "";
        } 
    }

    if parse_mode.is_empty() {
        if let Some(wasm_path) = wasm_path {
            Ok(RunOptions { wasm_path, out_path })
        } else {
            Err("a wasm file to be compiled must be provided".to_string())
        }
    } else {
        Err(format!("expected path after option `{}`", parse_mode))
    }
}
