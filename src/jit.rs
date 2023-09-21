// 1. jitdata .. holds everything about a function, such as funcinfo, codeinfo, codegendata, registerdata
// 2. funcinfo .. deeper higher level informaiton of function
// 3. codeinfo .. compiled function info, so machine code information of already compiled function(mcode pointer, entrypoint, ...)
// 4. codegendata .. function specific machine code data (such as mcodebase, mcodeptr, etc.)

mod codegendata;
mod codeinfo;
mod funcinfo;
mod jitdata;
