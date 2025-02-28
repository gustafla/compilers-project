use std::{
    env,
    fs::{self, File},
    io::Read,
    path::{self, Path, PathBuf},
    process,
};

pub fn assemble(
    assembly_code: &str,
    workdir: Option<impl AsRef<Path>>,
    tempfile_basename: impl AsRef<Path>,
    link_with_c: bool,
    extra_libraries: &[&str],
) -> Vec<u8> {
    let workdir = if let Some(workdir) = workdir {
        path::absolute(workdir).unwrap()
    } else {
        // TODO: This is insecure but works for a toy compiler such as ours
        // Real implementation should use tempfile-crate
        env::temp_dir().join("compiler")
    };
    assemble_impl(
        assembly_code,
        workdir,
        tempfile_basename,
        link_with_c,
        extra_libraries,
        |path| {
            let mut buf = Vec::new();
            let mut file = File::open(path).unwrap();
            file.read_to_end(&mut buf).unwrap();
            buf
        },
    )
}

fn assemble_impl<T>(
    assembly_code: &str,
    workdir: impl AsRef<Path>,
    tempfile_basename: impl AsRef<Path>,
    link_with_c: bool,
    extra_libraries: &[&str],
    take_output: impl Fn(PathBuf) -> T,
) -> T {
    let workdir = workdir.as_ref();
    let tempfile_basename = tempfile_basename.as_ref();

    let stdlib_asm = workdir.join("stdlib.s");
    let stdlib_obj = workdir.join("stdlib.o");
    let program_asm = workdir.join(tempfile_basename.with_extension("s"));
    let program_obj = workdir.join(tempfile_basename.with_extension("o"));
    let output_file = workdir.join("a.out");

    dbg!(&stdlib_asm);
    dbg!(&stdlib_obj);
    dbg!(&program_asm);
    dbg!(&program_obj);
    dbg!(&output_file);

    let final_stdlib_asm_code = if link_with_c {
        drop_start_symbol(STDLIB_ASM_CODE)
    } else {
        STDLIB_ASM_CODE.into()
    };

    fs::create_dir(workdir).ok();
    fs::write(&stdlib_asm, final_stdlib_asm_code).unwrap();
    fs::write(&program_asm, assembly_code).unwrap();

    process::Command::new("as")
        .arg("-g")
        .arg("-o")
        .arg(&stdlib_obj)
        .arg(stdlib_asm)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    process::Command::new("as")
        .arg("-g")
        .arg("-o")
        .arg(&program_obj)
        .arg(program_asm)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut linker_flags = vec![String::from("-static")];
    linker_flags.extend(extra_libraries.iter().map(|lib| format!("-l{lib}")));
    if link_with_c {
        process::Command::new("cc")
    } else {
        process::Command::new("ld")
    }
    .arg("-o")
    .arg(&output_file)
    .args(linker_flags)
    .arg(stdlib_obj)
    .arg(program_obj)
    .spawn()
    .unwrap()
    .wait()
    .unwrap();

    take_output(output_file)
}

fn drop_start_symbol(code: &str) -> String {
    let mut asm = String::from(code.split_once("# BEGIN START").unwrap().0);
    asm.push_str(code.split_once("# END START").unwrap().1);
    asm
}

static STDLIB_ASM_CODE: &str = include_str!("stdlib.s");
