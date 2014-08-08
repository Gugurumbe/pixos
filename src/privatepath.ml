let userdir = Filename.concat
  begin
    match Sys.os_type with
    | "Unix" -> Sys.getenv "HOME"
    | _ -> Sys.getenv "USERPROFILE"
  end
  ".pixos" ;;
let userprogress = Filename.concat userdir "progression" ;;
if not (Sys.file_exists userdir) 
then Unix.mkdir userdir 0o751 ;;
if not (Sys.file_exists userprogress) 
then Unix.mkdir userprogress 0o751 ;;
