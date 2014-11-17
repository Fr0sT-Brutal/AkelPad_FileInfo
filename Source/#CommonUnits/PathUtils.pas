unit PathUtils;

interface

type
  // path delimiters
  TPathType = (ptLocal, ptRemote);
  TPathKind = (pkDot, pkRelative, pkAbsolute{$IFDEF MSWINDOWS}, pkUNC{$ENDIF});
  TDelimPlace = (dpLead, dpTrail);
  TDelimAction = (daLeave, daSet, daRemove);
  TDelimPlaceActions = array[TDelimPlace] of TDelimAction;

const
  PathDelims: array [TPathType] of Char = ({$IFDEF MSWINDOWS}'\'{$ELSE}'/'{$ENDIF}, '/');
  {$IFDEF MSWINDOWS}
  DriveDelim = ':';
  UNCSign = '\\';
  {$ENDIF}
  DelimBoth: TDelimPlaceActions = (daSet, daSet);
  DelimNone: TDelimPlaceActions = (daRemove, daRemove);
  DelimAsIs: TDelimPlaceActions = (daLeave, daLeave);
  CurrDir = '.';
  ParentDir = '..';

// ** Checking delimiters ** \\

function HasLeadDelim(PathType: TPathType; const Path: string): Boolean; inline;
function HasLeadDelimL(const Path: string): Boolean; inline;
function HasLeadDelimR(const Path: string): Boolean; inline;

function HasTrailDelim(PathType: TPathType; const Path: string): Boolean; inline;
function HasTrailDelimL(const Path: string): Boolean; inline;
function HasTrailDelimR(const Path: string): Boolean; inline;

// ** Checking absolute/relative/"dot" pseudo-directory ** \\

function GetPathKind(PathType: TPathType; const Path: string): TPathKind;
//function IsRelativePath(PathType: TPathType; const Path: string): Boolean;
//function IsAbsolutePath(PathType: TPathType; const Path: string): Boolean;
//function IsDotDir(PathType: TPathType; const Path: string): Boolean;

// ** Ensuring delimiters ** \\

function Delims(Lead, Trail: TDelimAction): TDelimPlaceActions; inline;

function EnsureDelims(PathType: TPathType; Delims: TDelimPlaceActions; const Path: string): string;
function EnsureDelimsR(Delims: TDelimPlaceActions; const Path: string): string; inline;
function EnsureDelimsL(Delims: TDelimPlaceActions; const Path: string): string; inline;

procedure ChangeDelims(DestPathType: TPathType; var Path: string);
procedure ChangeDelimsR(var Path: string); inline;
procedure ChangeDelimsL(var Path: string); inline;

// ** Extracting path parts ** \\

function ExtractNameExt(PathType: TPathType; const Path: string): string;
function ExtractNameExtR(const Path: string): string; inline;
function ExtractNameExtL(const Path: string): string; inline;

function ExtractPath(PathType: TPathType; const Path: string): string;
function ExtractPathR(const Path: string): string; inline;
function ExtractPathL(const Path: string): string; inline;

function ExtractName(PathType: TPathType; const Path: string): string;
function ExtractNameR(const Path: string): string; inline;
function ExtractNameL(const Path: string): string; inline;

function ExtractExt(PathType: TPathType; const Path: string): string;
function ExtractExtR(const Path: string): string; inline;
function ExtractExtL(const Path: string): string; inline;

// ** Changing path parts ** \\

function ChangeName(PathType: TPathType; const Path, NewName: string): string;
function ChangeNameR(const Path, NewName: string): string; inline;
function ChangeNameL(const Path, NewName: string): string; inline;

function ChangeExt(PathType: TPathType; const Path, NewExt: string): string;
function ChangeExtR(const Path, NewExt: string): string; inline;
function ChangeExtL(const Path, NewExt: string): string; inline;

// ** Merging path parts into single path ** \\

function ConstructPath(PathType: TPathType; BoundDelims: TDelimPlaceActions; const Parts: array of string): string; overload;
function ConstructPathR(BoundDelims: TDelimPlaceActions; const Parts: array of string): string; overload;
function ConstructPathL(BoundDelims: TDelimPlaceActions; const Parts: array of string): string; overload;
function ConstructPath(PathType: TPathType; const Parts: array of string): string; overload;
function ConstructPathR(const Parts: array of string): string; overload;
function ConstructPathL(const Parts: array of string): string; overload;

implementation

// ** Checking delimiters ** \\

// Checks if Path starts with delimiter
function HasLeadDelim(PathType: TPathType; const Path: string): Boolean;
begin
  Result := (Path <> '') and (Path[1] = PathDelims[PathType]);
end;

function HasLeadDelimL(const Path: string): Boolean;
begin
  Result := HasLeadDelim(ptLocal, Path);
end;

function HasLeadDelimR(const Path: string): Boolean;
begin
  Result := HasLeadDelim(ptRemote, Path);
end;

// Checks if Path ends with delimiter
function HasTrailDelim(PathType: TPathType; const Path: string): Boolean;
begin
  Result := (Path <> '') and (Path[Length(Path)] = PathDelims[PathType]);
end;

function HasTrailDelimL(const Path: string): Boolean;
begin
  Result := HasTrailDelim(ptLocal, Path);
end;

function HasTrailDelimR(const Path: string): Boolean;
begin
  Result := HasTrailDelim(ptRemote, Path);
end;

// ** Checking dot/UNC/absolute/relative ** \\

// For remote and Unix local path types "/foo/bar" paths are considered absolute.
// For local Windows path type the path is considered relative when it doesn't
//   have drive delimiter : at index 2.
// Empty paths are considered relative
// UNC paths (Windows-only) start with \\
// Dot dirs are . and ..

function GetPathKind(PathType: TPathType; const Path: string): TPathKind;
var dir: string;
begin
  // is dot?
  if Length(Path) <= Length(ParentDir) + 1 then
  begin
    dir := EnsureDelims(PathType, DelimNone, Path);
    if (dir = CurrDir) or (dir = ParentDir) then
      Exit(pkDot);
  end;
  // is remote relative/absolute?
  if PathType = ptRemote then
  begin
    if HasLeadDelim(ptRemote, Path)
      then Exit(pkAbsolute)
      else Exit(pkRelative);
  end
  // is local UNC/relative/absolute?
  else
  begin
    // is UNC?
    {$IFDEF MSWINDOWS}
    if Copy(Path, 1, Length(UNCSign)) = UNCSign then
      Exit(pkUNC);
    {$ENDIF}
    // is relative/absolute?
    {$IFDEF MSWINDOWS}
    if ( (Length(Path) >= 2) and (Path[2] = DriveDelim) )
      then Exit(pkAbsolute)
      else Exit(pkRelative);
    {$ELSE}
    if HasLeadDelim(ptLocal, Path)
      then Exit(pkAbsolute)
      else Exit(pkRelative);
    {$ENDIF}
  end;
end;
{
// Determines whether the path is relative (empty paths are considered relative)
function IsRelativePath(PathType: TPathType; const Path: string): Boolean;
begin
  Result := GetPathKind(PathType, Path) = pkRelative;
end;

// Determines whether the path is absolute (empty paths are considered relative)
function IsAbsolutePath(PathType: TPathType; const Path: string): Boolean;
begin
  Result := GetPathKind(PathType, Path) = pkAbsolute;
end;

// Determines whether the path is "dot" pseudo-directory: "." or ".."
function IsDotDir(PathType: TPathType; const Path: string): Boolean;
begin
  Result := GetPathKind(PathType, Path) = pkDot;
end;
}
// ** Ensuring delimiters ** \\

// Searches for last delimiter occurence in Path, 0 if not found
function LastDelimiter(PathType: TPathType; const Path: string): Integer;
begin
  Result := Length(Path);

  while Result > 0 do
    if (Path[Result] = PathDelims[PathType])
       {$IFDEF MSWINDOWS}or ( (PathType = ptLocal) and (Path[Result] = DriveDelim) ){$ENDIF}
         then Exit
         else Dec(Result);
end;

// Ensures that Path has delimiters exactly in the needed places
function EnsureDelims(PathType: TPathType; Delims: TDelimPlaceActions; const Path: string): string;
begin
  Result := Path;
  // leading delim
  if Delims[dpLead] <> daLeave then
    if HasLeadDelim(PathType, Result) <> (Delims[dpLead] = daSet) then
      if Delims[dpLead] = daSet
        then Result := PathDelims[PathType] + Result
        else Delete(Result, 1, 1);
  // trailing delim
  if Delims[dpTrail] <> daLeave then
    if HasTrailDelim(PathType, Result) <> (Delims[dpTrail] = daSet) then
      if Delims[dpTrail] = daSet
        then Result := Result + PathDelims[PathType]
        else Delete(Result, Length(Result), 1);
end;

function EnsureDelimsR(Delims: TDelimPlaceActions; const Path: string): string;
begin
  Result := EnsureDelims(ptRemote, Delims, Path);
end;

function EnsureDelimsL(Delims: TDelimPlaceActions; const Path: string): string;
begin
  Result := EnsureDelims(ptLocal, Delims, Path);
end;

// Replaces all path delim occurences to a given path type delims (remote <--> local)
procedure ChangeDelims(DestPathType: TPathType; var Path: string);
var i: Integer;
    SrcPathType: TPathType;
begin
  case DestPathType of
    ptLocal  : SrcPathType := ptRemote;
    ptRemote : SrcPathType := ptLocal;
    else       Exit;
  end;
  for i := 1 to Length(Path) do
    if Path[i] = PathDelims[SrcPathType] then
      Path[i] := PathDelims[DestPathType];
end;

procedure ChangeDelimsR(var Path: string);
begin
  ChangeDelims(ptRemote, Path);
end;

procedure ChangeDelimsL(var Path: string);
begin
  ChangeDelims(ptLocal, Path);
end;

// Just a dumb pseudo-constructor because Delphi can't create typed array inline
function Delims(Lead, Trail: TDelimAction): TDelimPlaceActions;
begin
  Result[dpLead] := Lead;
  Result[dpTrail] := Trail;
end;

// ** Extracting path parts ** \\

// Determines positions of filename and extension parts of Path, 0 if not found
procedure GetPathPositions(PathType: TPathType; const Path: string; out NamePos, ExtPos: Integer);
var PathEnd: Integer;
begin
  NamePos := 0; ExtPos := 0;
  // check empty string
  if Path = '' then Exit;
  // Determine position of the last path delimiter
  PathEnd := LastDelimiter(PathType, Path);
  if PathEnd = Length(Path) then Exit; // Path is a folder
  NamePos := PathEnd + 1;              // Works also if Path has no path delims
  // search for the last "."
  ExtPos := Length(Path);
  while ExtPos >= NamePos do
    if Path[ExtPos] = '.'
      then Exit
      else Dec(ExtPos);
  // no extension
  ExtPos := 0;
end;

// Extracts name and ext from the path. Path must have no trailing delimiter ( path\to\file.ext => file.ext )
function ExtractNameExt(PathType: TPathType; const Path: string): string;
var I: Integer;
begin
  i := LastDelimiter(PathType, Path);
  if i = 0
    then Result := Path
    else Result := Copy(Path, i + 1, MaxInt);
end;

function ExtractNameExtR(const Path: string): string;
begin
  Result := ExtractNameExt(ptRemote, Path);
end;

function ExtractNameExtL(const Path: string): string;
begin
  Result := ExtractNameExt(ptLocal, Path);
end;

// Extracts file name without extension ( path\to\file.ext => file )
function ExtractName(PathType: TPathType; const Path: string): string;
var NamePos, ExtPos: Integer;
begin
  Result := '';
  GetPathPositions(PathType, Path, NamePos, ExtPos);
  if NamePos = 0 then Exit;  // Path is a folder
  if ExtPos = 0              // no ext
    then Result := Copy(Path, NamePos, MaxInt)
    else Result := Copy(Path, NamePos, ExtPos-NamePos);
end;

function ExtractNameR(const Path: string): string;
begin
  Result := ExtractName(ptRemote, Path);
end;

function ExtractNameL(const Path: string): string;
begin
  Result := ExtractName(ptLocal, Path);
end;

// Extracts extension ( path\to\file.ext => .ext )
function ExtractExt(PathType: TPathType; const Path: string): string;
var NamePos, ExtPos: Integer;
begin
  Result := '';
  GetPathPositions(PathType, Path, NamePos, ExtPos);
  if NamePos = 0 then Exit;  // Path is a folder
  if ExtPos = 0 then Exit;   // no ext
  Result := Copy(Path, ExtPos, MaxInt);
end;

function ExtractExtR(const Path: string): string;
begin
  Result := ExtractExt(ptRemote, Path);
end;

function ExtractExtL(const Path: string): string;
begin
  Result := ExtractExt(ptLocal, Path);
end;

// Extracts path to the parent folder from the full path. Path must have no trailing delimiter ( path\to\file.ext => path\to\ )
function ExtractPath(PathType: TPathType; const Path: string): string;
begin
  Result := Copy(Path, 1, LastDelimiter(PathType, Path));
end;

function ExtractPathR(const Path: string): string;
begin
  Result := ExtractPath(ptRemote, Path);
end;

function ExtractPathL(const Path: string): string;
begin
  Result := ExtractPath(ptLocal, Path);
end;

// ** Changing path parts ** \\

// Changes the name of a file leaving path and ext unchanged ( path\to\file.ext => path\to\newfile.ext )
function ChangeName(PathType: TPathType; const Path, NewName: string): string;
var NamePos, ExtPos: Integer;
begin
  Result := Path;
  GetPathPositions(PathType, Path, NamePos, ExtPos);
  if NamePos = 0 then Exit;  // Path is a folder
  if ExtPos = 0              // no ext
    then Result := Copy(Path, 1, NamePos - 1) + NewName
    else Result := Copy(Path, 1, NamePos - 1) + NewName + Copy(Path, ExtPos, MaxInt);
end;

function ChangeNameR(const Path, NewName: string): string;
begin
  Result := ChangeName(ptRemote, Path, NewName);
end;

function ChangeNameL(const Path, NewName: string): string;
begin
  Result := ChangeName(ptLocal, Path, NewName);
end;

// Changes the ext of a file leaving path and name unchanged ( path\to\file.ext => path\to\file.new )
// NewExt is new extension with or without dot
function ChangeExt(PathType: TPathType; const Path, NewExt: string): string;
var NamePos, ExtPos: Integer;
    Ext: string;
begin
  Result := Path;
  GetPathPositions(PathType, Path, NamePos, ExtPos);
  if NamePos = 0 then Exit;  // Path is a folder
  // add dot if NewExt doesn't contain it
  if (NewExt <> '') and (NewExt[1] <> '.')
    then Ext := '.' + NewExt
    else Ext := NewExt;
  if ExtPos = 0              // no ext
    then Result := Path + Ext
    else Result := Copy(Path, 1, ExtPos - 1) + Ext;
end;

function ChangeExtR(const Path, NewExt: string): string; inline;
begin
  Result := ChangeExt(ptRemote, Path, NewExt);
end;

function ChangeExtL(const Path, NewExt: string): string; inline;
begin
  Result := ChangeExt(ptLocal, Path, NewExt);
end;

// ** Merging path parts into single path ** \\

// Merges path parts into one path, taking care of any delimiters in parts.
// Can add bounding delimiters to the resulting path
function ConstructPath(PathType: TPathType; BoundDelims: TDelimPlaceActions; const Parts: array of string): string;
var i, start: Integer;
begin
  Result := '';
  if Length(Parts) = 0 then Exit; // empty array - go away

  // search for 1st not-empty element
  start := 0;
  while start < Length(Parts) do
    if Parts[start] = ''
      then Inc(start)
      else Break;

  // set initial value
  Result := Parts[start];

  // run thru other elements
  for i := start+1 to Length(Parts)-1 do
  begin
    if Parts[i] = '' then Continue; // empty - skip it
    if HasTrailDelim(PathType, Result) then
      if HasLeadDelim(PathType, Parts[i]) then
        Result := Result + Copy(Parts[i], 2, MaxInt)
      else
        Result := Result + Parts[i]
    else
      if HasLeadDelim(PathType, Parts[i]) then
        Result := Result + Parts[i]
      else
        Result := Result + PathDelims[PathType] + Parts[i];
  end;

  // add bounding delimiters if needed
  Result := EnsureDelims(PathType, BoundDelims, Result);
end;

function ConstructPathR(BoundDelims: TDelimPlaceActions; const Parts: array of string): string;
begin
  Result := ConstructPath(ptRemote, BoundDelims, Parts);
end;

function ConstructPathL(BoundDelims: TDelimPlaceActions; const Parts: array of string): string;
begin
  Result := ConstructPath(ptLocal, BoundDelims, Parts);
end;

// Construct path with both delims unchanged

function ConstructPath(PathType: TPathType; const Parts: array of string): string;
begin
  Result := ConstructPath(PathType, DelimAsIs, Parts);
end;

function ConstructPathR(const Parts: array of string): string; overload;
begin
  Result := ConstructPath(ptRemote, DelimAsIs, Parts);
end;

function ConstructPathL(const Parts: array of string): string; overload;
begin
  Result := ConstructPath(ptLocal, DelimAsIs, Parts);
end;

end.
