let userdir = Filename.concat
  begin
    match Sys.os_type with
    | "Unix" -> Sys.getenv "HOME"
    | _ -> Sys.getenv "USERPROFILE"
  end
  ".pixos" ;;

if not (Sys.file_exists userdir) 
then Unix.mkdir userdir 0o751 ;;
