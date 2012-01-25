(*****************************************
    FileInfo plugin for AkelPad editor
                 © Fr0sT

  Shows properties of a currently edited file
  as long as some its contents statistics.
  Something similar to Stats plugin but provides
  more info.

*****************************************)

library FileInfo;

{$R 'Dialog.res' 'Dialog.rc'}
{$R *.RES}

uses
  Windows, Messages, SysUtils, CommCtrl, ShellApi,
  IceUtils, ResDialog,
  AkelDLL_h  in '#AkelDefs\AkelDLL_h.pas',
  AkelEdit_h in '#AkelDefs\AkelEdit_h.pas';

// Global constants

const
  PluginName: AnsiString = 'FileInfo';

type
  // statistics that are counted in the separate thread
  TDocCountStats = record
    Lines,                     // Without word wrap
    Chars,                     // Total chars
    CharsSpace,                // Spaces, tabs, etc
    Words,
    whatever: Int64;
  end;

  // full set of file and document properties and stats
  TFileStats = record
    // file props
    FileName: string;          // Full file path
    FileSize: Int64;
    Created,
    Modified: TDateTime;
    hIcon: HICON;              // Shell icon handle
    // general document info
    CodePage: Integer;         //
    // counters
    Counters: TDocCountStats;
  end;

  // internal data and structures for counting

  TCountState = (stNotStarted, stWords, stChars, stFinished);

  TCountData = record
    hMainWnd, hEditWnd: HWND;                   // main window and edit window
    crInit: TAECHARRANGE;                       //   data
    crCount: TAECHARRANGE;                      //
    ciWordStart: TAECHARINDEX;                  //         for
    ciWordEnd: TAECHARINDEX;                    //
    ciCount: TAECHARINDEX;                      //
    isChars: TAEINDEXSUBTRACT;                  //            counting
    Selection, Wrap, ColumnSel: Boolean;        // current document modes
    CountState: TCountState;                    // what we're counting now
    CharsProcessed: Int64;                      // how much chars have been already processed
  end;
  PCountData = ^TCountData;

{$REGION 'Localization'}

// String resources
type
  TStringID = 
  (
    // dialog window labels and captions
    idTitleFileProps,
    idTitleSelProps,
    idPgFileTitle,
    idPgDocTitle,
    idPgFileLabelPath,
    idPgFileLabelSize,
    idPgFileLabelCreated,
    idPgFileLabelModified,
    idPgDocLabelCodePage,
    idPgDocLabelLines,
    idPgDocLabelChars,
    idPgDocLabelWords,
    idPgDocLabelCharsNoSp,
    idPgDocLabelSmth,
    idPgDocBtnCount,
    idPgDocBtnAbort,
    idMainBtnOK,
    //...
    // messages
    idMsgGetPropsFail,
    idMsgShowDlgFail,
    // other
    idBla
  );
  TLangStrings = array[TStringID] of string;
  TLangData = record
    LangId: LANGID;
    Strings: TLangStrings;
  end;
const
  LangData: array[1..2] of TLangData =
  (
    // ru
    (
      LangId: LANG_RUSSIAN;
      Strings: (
        'Свойства файла',
        'Свойства выделенного фрагмента',
        'Файл',
        'Текст',
        'Путь',
        'Размер',
        'Создан',
        'Изменён',
        'Кодировка',
        'Строки',
        'Символы',
        'Слова',
        'Символы без пробелов',
        'Что-то',
        'Подсчитать',
        'Прервать',
        'ОК',
        'Ошибка при получении свойств',
        'Ошибка при показе диалога',
        'Блабла'
      );
    ),
    // en
    (
      LangId: LANG_ENGLISH;
      Strings: (
        'File statistics',
        'Selection statistics',
        'File',
        'Text',
        'Path',
        'Size',
        'Created',
        'Modified',
        'Codepage',
        'Lines',
        'Chars',
        'Words',
        'Chars without spaces',
        'Smth',
        'Count',
        'Abort',
        'OK',
        'Error retrieving statistics',
        'Error showing the dialog',
        'Blabla'
      );
    )
  );
var
  CurrLangId: LANGID = LANG_NEUTRAL;

// Returns a string with given ID corresponding to current langID
function LangString(StrId: TStringID): string;
var i: Integer;
begin
  for i := Low(LangData) to High(LangData) do
    if LangData[i].LangId = CurrLangId then
      Exit(LangData[i].Strings[StrId]);
  // lang ID not found - switch to English and re-run
  CurrLangId := LANG_ENGLISH;
  Result := LangString(StrId);
end;

{$ENDREGION}

// Interface

{$I Dialog.inc}  // dialog IDs

type
  // current counting state
  TCountProgress = record
    PercentDone: Cardinal;
    Counters: TDocCountStats;
  end;
  PCountProgress = ^TCountProgress;

  // counter thread related data
  TCountThreadData = record
    hThread: THandle;
    idThread: DWORD;
    hTargetWnd: HWND;
    Terminated: Boolean;
    Running: Boolean;
  end;

  // tab pages
  TTabPage = (tabFile, tabDoc);

  // dialog classes
  TPageDlg = class;

  TMainDlg = class(TResTextDialog)
  strict private
    FPages: array[TTabPage] of TPageDlg;
    FAppIcon: HICON;
    FTabPageCaptions: array[TTabPage] of string;
    procedure DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT); override;
  public
    constructor Create(const pd: TPLUGINDATA);
    destructor Destroy; override;
  end;

  TPageDlg = class(TResTextDialog)
  strict private
    FOwner: TMainDlg;
    FPage: TTabPage;
    FValuesWereSet: Boolean;
    procedure DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT); override;
  public
    constructor Create(const pd: TPLUGINDATA; Owner: TMainDlg; Page: TTabPage);
    procedure SetValues;
  end;

const
  WordsPerCycle = 100;  {}
  CharsPerCycle = 2000; {}
  TabPageIDs: array[TTabPage] of Integer =
    (IDD_INFO_FILE, IDD_INFO_DOC);

  // Count thread -> main dialog window
  //   wParam: thread state - TS_*
  //   lParam: pointer to TCountProgress record
  MSG_UPD_COUNT = WM_USER + $FF1;
  // count thread states
  TS_TERMINATED = DWORD(-1);
  TS_FINISHED   =         0;
  TS_ACTIVE     =         1;

// Global variables

var
  FileStats: TFileStats;
  dlgMain: TMainDlg;
  ThreadData: TCountThreadData; // ! this record is shared between main and counter threads !
  CountData: TCountData;

// ***** SERVICE FUNCTIONS ***** \\

// *** Functions that get file and document info ***

// Retrieve file properties.
function GetFileInfo(var CountData: TCountData; var FileStats: TFileStats): Boolean;
var ei: TEDITINFO;
    ShInf: TSHFileInfo;
begin
  DestroyIcon(FileStats.hIcon);
  Finalize(FileStats);
  ZeroMem(FileStats, SizeOf(FileStats));

  ZeroMem(ei, SizeOf(ei));
  if (CountData.hMainWnd = 0) or
     (CountData.hEditWnd = 0) or
     (SendMessage(CountData.hMainWnd, AKD_GETEDITINFO, 0, LPARAM(@ei)) = 0) then
       Exit(False);

  if ei.wszFile <> nil then
    FileStats.FileName := string(ei.wszFile)
  else if ei.szFile <> nil then
    FileStats.FileName := string(AnsiString(ei.szFile));
  if FileStats.FileName <> '' then  {}
  begin
    FileStats.FileSize := GetFileSize(FileStats.FileName);
    GetFileTime(FileStats.FileName, tsLoc, @FileStats.Created, nil, @FileStats.Modified);
    ZeroMem(ShInf, SizeOf(ShInf));
    SHGetFileInfo(PChar(FileStats.FileName), FILE_ATTRIBUTE_NORMAL, ShInf, SizeOf(ShInf),
                  SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_LARGEICON); {}//check result
//    if ShInf.hIcon then

    FileStats.hIcon := ShInf.hIcon;
    {}// load empty icon
  end;
  {} // show "(not a file)" or even hide the page
  {}// unsaved files - ?

  FileStats.CodePage := ei.nCodePage;
  Result := True;
end;

// Retrieve document properties.
// Acts on the basis of CountData.CountState (and changes it when count stage is changing).
// When counting, processes only a tiny part of the text allowing progress reporting and
//   termination of the calling thread.
procedure GetDocInfo(var CountData: TCountData; var CountProgress: TCountProgress);
var line1, line2, WordCnt, CharCnt, tmp: Integer;
    NotLineEnd: Boolean;
    CurrChar: WideChar;
begin
  case CountData.CountState of
    // count not started or has been finished - get basic document info and start a new count
    stNotStarted, stFinished:
      begin
        ZeroMem(CountProgress, SizeOf(CountProgress));

        SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTSELCHAR, LPARAM(@CountData.crInit.ciMin));
        SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_LASTSELCHAR,  LPARAM(@CountData.crInit.ciMax));

        // Check if there's selection and wrapping present
        CountData.Selection := (AEC_IndexCompare(CountData.crInit.ciMin, CountData.crInit.ciMax) <> 0);
        CountData.Wrap := SendMessage(CountData.hEditWnd, AEM_GETWORDWRAP, 0, LPARAM(nil)) <> 0;
        CountData.ColumnSel := SendMessage(CountData.hEditWnd, AEM_GETCOLUMNSEL, 0, 0) <> 0;

        // lines count
        if not CountData.Selection then
        begin
          SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTCHAR, LPARAM(@CountData.crInit.ciMin));
          SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_LASTCHAR, LPARAM(@CountData.crInit.ciMax));
        end;
        line1 := CountData.crInit.ciMin.nLine;
        line2 := CountData.crInit.ciMax.nLine;
        if CountData.Wrap then
        begin
          line1 := SendMessage(CountData.hEditWnd, AEM_GETUNWRAPLINE, WPARAM(line1), 0);
          line2 := SendMessage(CountData.hEditWnd, AEM_GETUNWRAPLINE, WPARAM(line2), 0);
        end;
        CountProgress.Counters.Lines := line2 - line1 + 1;

        {}// selection present, if caret is on the 1st char - excess line

        // chars count
        CountData.isChars.ciChar1 := @CountData.crInit.ciMax;
        CountData.isChars.ciChar2 := @CountData.crInit.ciMin;
        CountData.isChars.bColumnSel := BOOL(-1);
        CountData.isChars.nNewLine := AELB_ASOUTPUT;
        CountProgress.Counters.Chars := SendMessage(CountData.hEditWnd, AEM_INDEXSUBTRACT, 0, LPARAM(@CountData.isChars));

        // init data for word count
        CountProgress.PercentDone := 0;
        CountData.CharsProcessed := 0;
        if not CountData.Selection then
          CountData.ciCount := CountData.crInit.ciMin
        else
        begin
          CountData.crCount.ciMin := CountData.crInit.ciMin;
          CountData.crCount.ciMax := CountData.crInit.ciMax;
        end;
ZeroMem(CountData.ciWordStart, SizeOf(CountData.ciWordStart));
ZeroMem(CountData.ciWordEnd,   SizeOf(CountData.ciWordEnd));

        // switch to the next count stage
        CountData.CountState := stWords;
      end;

    // words count
    stWords:
      if not CountData.Selection then
      begin
        WordCnt := 0; CharCnt := 0; // how much words/chars we've processed during current cycle

        repeat
          // returns number of characters skipped to the next word
          tmp := SendMessage(CountData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@CountData.ciCount));
          // EOF - finish the cycle
          if tmp = 0 then
          begin
            CountProgress.PercentDone := 100;
            Break;
          end;
          Inc(CharCnt, tmp);
          Inc(WordCnt);
        until WordCnt > WordsPerCycle;

        // still in progress - get current percent value
        if CountProgress.PercentDone < 100 then
        begin
          Inc(CountProgress.Counters.Words, WordCnt);
          Inc(CountData.CharsProcessed, CharCnt);
          CountProgress.PercentDone := Trunc(CountData.CharsProcessed*100/CountProgress.Counters.Chars);
          if CountProgress.PercentDone > 99 then // we'll reach 100% only when AEM_GETNEXTBREAK return 0
            CountProgress.PercentDone := 99;
        end
        // word count finished, prepare data for the next stage and switch to it
        else
        begin
        {}ZeroMem(CountData.crCount,     SizeOf(CountData.crCount));
          CountData.ciCount := CountData.crInit.ciMin;
          CountData.CharsProcessed := 0;

          // switch to the next count stage
          CountData.CountState := stChars;
        end;
      end
      else
        if CountData.ColumnSel then
        begin
          while (AEC_IndexCompare(CountData.crCount.ciMin, CountData.crCount.ciMax) < 0) do
          begin
            CountData.ciWordEnd := CountData.crCount.ciMin;
            repeat
              // returns number of characters skipped to the next word
              tmp := SendMessage(CountData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@CountData.ciWordEnd));
              // EOF - finish the cycle
              if tmp = 0 then
              begin
                CountProgress.PercentDone := 100;
                Break;
              end;

              // word ends beyond the selection - finish the cycle
              if not ((CountData.ciWordEnd.nLine = CountData.crCount.ciMin.nLine) and
                      (CountData.ciWordEnd.nCharInLine <= CountData.crCount.ciMin.lpLine.nSelEnd)) then Break;
        {
              if not bFirst then
                Inc(WordCnt)
              else
              begin
                bFirst := False;
                CountData.ciWordStart := CountData.ciWordEnd;
                if SendMessage(CountData.hEditWnd, AEM_GETPREVBREAK, AEWB_LEFTWORDSTART, LPARAM(@CountData.ciWordStart)) <> 0 then
                  if AEC_IndexCompare(CountData.crCount.ciMin, CountData.ciWordStart) <= 0 then
                    Inc(WordCnt);
              end;
        }
              // word ends on the end of selection - finish the cycle (?)
              if CountData.ciWordEnd.nCharInLine = CountData.crCount.ciMin.lpLine.nSelEnd then
                Break;

              Inc(CharCnt, tmp);
            until WordCnt > WordsPerCycle; {}//

            //Next line
       {     bFirst := True;
            if AEC_NextLine(CountData.crCount.ciMin) then
              CountData.crCount.ciMin.nCharInLine := CountData.crCount.ciMin.lpLine.nSelStart;
       }   end;
        end
        else
        begin
          repeat
            CountData.ciWordEnd := CountData.crCount.ciMin;
            // returns number of characters skipped to the next word
            tmp := SendMessage(CountData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@CountData.ciWordEnd));
            // EOF - finish the cycle
            if tmp = 0 then
            begin
              CountProgress.PercentDone := 100;
              Break;
            end;


          until WordCnt > WordsPerCycle;

(*

          for (;;)
          {

            if (AEC_IndexCompare(&ciWordEnd, &crCount.ciMax) <= 0)
            {
              if (bFirst)
              {
                bFirst=FALSE;
                ciWordStart=ciWordEnd;

                if (SendMessage(hWndEdit, AEM_GETPREVBREAK, AEWB_LEFTWORDSTART, (LPARAM)&ciWordStart))
                  if (AEC_IndexCompare(&crCount.ciMin, &ciWordStart) <= 0)
                    ++nWordCount;
              }
              else ++nWordCount;

              if (AEC_IndexCompare(&ciWordEnd, &crCount.ciMax) == 0)
                break;
            }
            else break;

            //Next word
            crCount.ciMin=ciWordEnd;
          }


*)
        end;

    // chars count
    stChars:
      begin
        CharCnt := 0; // how much chars we've processed during current cycle

        repeat
          // EOF - finish the cycle
          if AEC_IndexCompare(CountData.ciCount, CountData.crInit.ciMax) >= 0 then
          begin
            CountProgress.PercentDone := 100;
            Break;
          end;

          AEC_NextChar(CountData.ciCount);
          Inc(CharCnt);

          if CountData.Selection then
            if CountData.ciCount.nCharInLine < CountData.ciCount.lpLine.nSelStart then
              CountData.ciCount.nCharInLine := CountData.ciCount.lpLine.nSelStart;

          if not CountData.Selection
            then NotLineEnd := (CountData.ciCount.nCharInLine < CountData.ciCount.lpLine.nLineLen)
            else NotLineEnd := (CountData.ciCount.nCharInLine < CountData.ciCount.lpLine.nLineLen) and
                         (CountData.ciCount.nCharInLine < CountData.ciCount.lpLine.nSelEnd);

          if NotLineEnd then
          begin
            //if AEC_IndexLen(CountData.ciCount) = 1 then
            CurrChar := CountData.ciCount.lpLine.wpLine[CountData.ciCount.nCharInLine];
            case CurrChar of
              #$0009, ' ': Inc(CountProgress.Counters.CharsSpace);
              {}//...
            end;

          end
          else
          begin
            {if (ciCount.lpLine->nLineBreak == AELB_R || ciCount.lpLine->nLineBreak == AELB_N)
              ++nCharLatinOther;
            else if (ciCount.lpLine->nLineBreak == AELB_RN)
              nCharLatinOther+=2;
            else if (ciCount.lpLine->nLineBreak == AELB_RRN)
              nCharLatinOther+=3;
            }
            AEC_NextLine(CountData.ciCount);
          end;

        until CharCnt > CharsPerCycle;

        // still in progress - get current percent value
        if CountProgress.PercentDone < 100 then
        begin
          Inc(CountData.CharsProcessed, CharCnt);
          CountProgress.PercentDone := Trunc(CountData.CharsProcessed*100/CountProgress.Counters.Chars);
          if CountProgress.PercentDone > 99 then // we'll reach 100% only when read all the file/selection
            CountProgress.PercentDone := 99;
        end
        // char count finished, prepare data for the next stage and switch to it
        else
        begin
          // switch to the next count stage
          CountData.CountState := stFinished;
        end;



        (*
          {
            if (AEC_IndexLen(&ciCount) == 1)
            {
              if (ciCount.lpLine->wpLine[ciCount.nCharInLine] <= 0x80)
              {
                if (ciCount.lpLine->wpLine[ciCount.nCharInLine] == L' ' ||
                    ciCount.lpLine->wpLine[ciCount.nCharInLine] == L'\t')
                {
                  ++nCharLatinSpaces;
                }
                else if ((ciCount.lpLine->wpLine[ciCount.nCharInLine] >= L'A' &&
                          ciCount.lpLine->wpLine[ciCount.nCharInLine] <= L'Z') ||
                         (ciCount.lpLine->wpLine[ciCount.nCharInLine] >= L'a' &&
                          ciCount.lpLine->wpLine[ciCount.nCharInLine] <= L'z'))
                {
                  ++nCharLatinLetters;
                }
                else if (ciCount.lpLine->wpLine[ciCount.nCharInLine] >= L'0' &&
                         ciCount.lpLine->wpLine[ciCount.nCharInLine] <= L'9')
                {
                  ++nCharLatinDigits;
                }
                else ++nCharLatinOther;
              }
              else ++nCharNonLatin;
            }
            else ++nCharSurrogate;
          }
        *)
      end;
  end;
end;

// *** Thread functions ***

// Main procedure of the counter thread
function ThreadProc(lParameter: Pointer): DWORD; stdcall;
var
  CountProgress: TCountProgress;
  CntData: TCountData;
  PrevPercent: Byte;
begin
  CntData := PCountData(lParameter)^;
  ThreadData.Running := True;
  PrevPercent := 0;

  // thread cycle
  while not ThreadData.Terminated do
  begin
    GetDocInfo(CntData, CountProgress);
    // signal the main window about increased percent
    if PrevPercent <> CountProgress.PercentDone then
    begin
      SendMessage(ThreadData.hTargetWnd, MSG_UPD_COUNT, TS_ACTIVE, LPARAM(@CountProgress));
      PrevPercent := CountProgress.PercentDone;
    end;
    // counting finished
    if CntData.CountState = stFinished then Break;
  end;

  // if exiting normally, return "OK", error otherwise
  Result := IfTh(ThreadData.Terminated, TS_TERMINATED, TS_FINISHED);
  // Send final count state
  SendMessage(ThreadData.hTargetWnd, MSG_UPD_COUNT, Result, LPARAM(@CountProgress));
  // clear the data
  CloseHandle(ThreadData.hThread);
  ZeroMem(ThreadData, SizeOf(ThreadData));
end;

// Launch the counting thread
procedure StartCountThread(TargetWnd: HWND; const CntData: TCountData);
begin
  if ThreadData.Running then Exit;
  ZeroMem(ThreadData, SizeOf(ThreadData));
  ZeroMem(FileStats.Counters, SizeOf(FileStats.Counters));
  ThreadData.hTargetWnd := TargetWnd;
  ThreadData.hThread := CreateThread(nil, 0, @ThreadProc, @CntData, 0, ThreadData.idThread);
  {}//SetThreadPriority
end;

// Stop the counting thread (doesn't wait for it, thread could run for some time
// after this procedure is finished!)
procedure StopCountThread;
begin
  if not ThreadData.Running then Exit;
  ThreadData.Terminated := True;
end;

// TMainDlg

constructor TMainDlg.Create(const pd: TPLUGINDATA);
var pg: TTabPage;
begin
  inherited Create(pd.hInstanceDLL, IDD_DLG_MAIN, pd.hMainWnd);

  FAppIcon := pd.hMainIcon;
  Caption := LangString(idTitleFileProps); {}
  ItemText[IDC_BTN_OK] := LangString(idMainBtnOK);

  FTabPageCaptions[tabFile] := LangString(idPgFileTitle);
  FTabPageCaptions[tabDoc] := LangString(idPgDocTitle);

  for pg := Low(TTabPage) to High(TTabPage) do
  begin
    FPages[pg] := TPageDlg.Create(pd, Self, pg);
    FPages[pg].Persistent := True;
  end;
end;

destructor TMainDlg.Destroy;
var pg: TTabPage;
begin
  for pg := Low(TTabPage) to High(TTabPage) do
    FreeAndNil(FPages[pg]);
  inherited;
end;

procedure TMainDlg.DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
var hwndTab, hwndPb: HWND;
    pg: TTabPage;
    tabItem: TTCItem;
    NotifyHdr: TNMHdr;
    progr: TCountProgress;
begin
  case msg of
    // dialog created and is going to be shown
    RDM_DLGOPENING:
      begin
        SendMessage(DlgHwnd, WM_SETICON, ICON_SMALL, Windows.LPARAM(FAppIcon));

        hwndTab := GetDlgItem(DlgHwnd, IDC_TAB);
        // init tabs
        for pg := Low(TTabPage) to High(TTabPage) do
        begin
          // add pages to tab control
          ZeroMem(tabItem, SizeOf(tabItem));
          tabItem.mask := TCIF_TEXT;
          tabItem.pszText := PChar(FTabPageCaptions[pg]);
          SendMessage(hwndTab, TCM_INSERTITEM, Integer(pg), Windows.LPARAM(@tabItem));
          // parent window handle changes every time so set the actual one
          FPages[pg].ParentHwnd := hwndTab;
        end;

        // imitate page change to init the page dialog
        NotifyHdr.hwndFrom := DlgHwnd;
        NotifyHdr.idFrom := IDC_TAB;
        NotifyHdr.code := TCN_SELCHANGE;
        SendMessage(DlgHwnd, WM_NOTIFY, 0, Windows.LPARAM(@NotifyHdr));

        FPages[tabDoc].ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnAbort);
        StartCountThread(DlgHwnd, CountData); // start counting
      end;

    // dialog is closing - stop the thread
    RDM_DLGCLOSING:
      begin
        StopCountThread;
      end;

    // tab page changes
    WM_NOTIFY:
      begin
        NotifyHdr := PNMHdr(lParam)^;
        case NotifyHdr.code of
          // page is about to change, hide the current page
          TCN_SELCHANGING:
            begin
              hwndTab := GetDlgItem(DlgHwnd, NotifyHdr.idFrom);
              pg := TTabPage(SendMessage(hwndTab, TCM_GETCURSEL, 0, 0));
              if not FPages[pg].Show(SW_HIDE) then
                MsgBox(LastErrMsg);
              Res := LRESULT(False); // must return false to allow page change
            end;
          // page was changed, show the current page
          TCN_SELCHANGE:
            begin
              hwndTab := GetDlgItem(DlgHwnd, NotifyHdr.idFrom);
              pg := TTabPage(SendMessage(hwndTab, TCM_GETCURSEL, 0, 0));
              if not FPages[pg].Show(SW_NORMAL) then
                MsgBox(LastErrMsg);
              Res := LRESULT(True);
            end;
          else
        end; // case NotifyHdr
      end;

    // message from counting thread - update progress
    MSG_UPD_COUNT:
      begin
        progr := PCountProgress(lParam)^;
        FileStats.Counters := progr.Counters;
        // thread is not active, change button caption
        if wParam <> TS_ACTIVE then
          FPages[tabDoc].ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnCount);

        // change values on the doc page only if it is visible
        if not IsWindowVisible(FPages[tabDoc].DlgHwnd) then Exit;
        FPages[tabDoc].SetValues;
        hwndPb := GetDlgItem(FPages[tabDoc].DlgHwnd, IDC_PGB_PROCESS);
        // return progress bar to zero if thread is not active
        if (progr.PercentDone >= 100) or (wParam <> TS_ACTIVE) then
          SendMessage(hwndPb, PBM_SETPOS, 0, 0)
        else
          SendMessage(hwndPb, PBM_SETPOS, progr.PercentDone, 0);

        Res := LRESULT(True);
      end;

    WM_COMMAND:
      begin
//  mlog.AddLine(mkinfo, 'main command');

      end;

  end; // case msg
end;

// TPageDlg

constructor TPageDlg.Create(const pd: TPLUGINDATA; Owner: TMainDlg; Page: TTabPage);
begin
  inherited Create(pd.hInstanceDLL, TabPageIDs[Page], pd.hMainWnd);
  FOwner := Owner;
  FPage := Page;
  case FPage of
    tabFile:
        SetItemTexts([
              ItemData(IDC_STC_FILEPATH, LangString(idPgFileLabelPath)),
              ItemData(IDC_STC_FILESIZE, LangString(idPgFileLabelSize)),
              ItemData(IDC_STC_CREATED,  LangString(idPgFileLabelCreated)),
              ItemData(IDC_STC_MODIFIED, LangString(idPgFileLabelModified)),

              ItemData(IDC_EDT_FILENAME, ''),
              ItemData(IDC_EDT_FILEPATH, ''),
              ItemData(IDC_EDT_CREATED, ''),
              ItemData(IDC_EDT_MODIFIED, '')
             ]);
    tabDoc:
        SetItemTexts([
              ItemData(IDC_STC_CODEPAGE,  LangString(idPgDocLabelCodePage)),
              ItemData(IDC_STC_LINES,     LangString(idPgDocLabelLines)),
              ItemData(IDC_STC_CHARS,     LangString(idPgDocLabelChars)),
              ItemData(IDC_STC_WORDS,     LangString(idPgDocLabelWords)),
              ItemData(IDC_STC_CHARSNOSP, LangString(idPgDocLabelCharsNoSp)),
              ItemData(IDC_STC_SMTH,      LangString(idPgDocLabelSmth)),
              ItemData(IDC_BTN_STOP,      LangString(idPgDocBtnCount)),

              ItemData(IDC_EDT_CODEPAGE,  ''),
              ItemData(IDC_EDT_LINES,     ''),
              ItemData(IDC_EDT_CHARS,     ''),
              ItemData(IDC_EDT_WORDS,     ''),
              ItemData(IDC_EDT_CHARSNOSP, ''),
              ItemData(IDC_EDT_CHARS,     '')
             ]);
  end;
end;

// set text values form FileStats record
procedure TPageDlg.SetValues;
var hwndItem: HWND;
begin
  case FPage of
    tabFile:
      if not FValuesWereSet then // only once because values won't change
      begin
        ItemText[IDC_EDT_FILENAME] := ExtractFileName(FileStats.FileName);
        ItemText[IDC_EDT_FILEPATH] := FileStats.FileName;
        ItemText[IDC_EDT_FILESIZE] := IfTh(FileStats.FileSize <> 0, ThousandsDivide(FileStats.FileSize), '');
        ItemText[IDC_EDT_CREATED]  := IfTh(FileStats.Created <> 0,  DateTimeToStr(FileStats.Created), '');
        ItemText[IDC_EDT_MODIFIED] := IfTh(FileStats.Modified <> 0, DateTimeToStr(FileStats.Modified), '');

        hwndItem := GetDlgItem(DlgHwnd, IDC_IMG_FILEICON);
        SendMessage(hwndItem, STM_SETICON, WPARAM(FileStats.hIcon), 0);

        FValuesWereSet := True;
      end;
    tabDoc:
      begin
        if not FValuesWereSet then // some values might change (during count process) and some might not
        begin
          ItemText[IDC_EDT_CODEPAGE]  := IntToStr(FileStats.CodePage); {}
          FValuesWereSet := True;
        end;
        // update these values always
        ItemText[IDC_EDT_LINES]     := ThousandsDivide(FileStats.Counters.Lines);
        ItemText[IDC_EDT_CHARS]     := ThousandsDivide(FileStats.Counters.Chars);
        ItemText[IDC_EDT_CHARSNOSP] := ThousandsDivide(FileStats.Counters.Chars - FileStats.Counters.CharsSpace);
        ItemText[IDC_EDT_WORDS]     := ThousandsDivide(FileStats.Counters.Words);
        ItemText[IDC_EDT_SMTH]      := ThousandsDivide(FileStats.Counters.whatever);
      end;
  end;

end;

procedure TPageDlg.DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
var
  hwndTab: HWND;
  TabClientArea: TRect;
begin
  case msg of
    // dialog is showing - set position inside tab page and load values to controls
    RDM_DLGOPENING:
      begin
        // calculate tab's client area
        hwndTab := ParentHwnd;
        GetClientRect(hwndTab, TabClientArea);
        SendMessage(hwndTab, TCM_ADJUSTRECT, Windows.WPARAM(False), Windows.LPARAM(@TabClientArea));
        SetWindowPos(DlgHwnd, 0, TabClientArea.Left, TabClientArea.Top,
                     TabClientArea.Right - TabClientArea.Left, TabClientArea.Bottom - TabClientArea.Top,
                     SWP_NOZORDER);
        SetValues;
      end;

    RDM_DLGCLOSED:
;
//        mlog.AddLine(mkinfo, itos(integer(sender)) + ' closed');

    // notification from control
    WM_COMMAND:
      case HiWord(wParam) of
        BN_CLICKED:
          case LOWORD(wParam) of
            // start/stop counting button
            IDC_BTN_STOP:
              if ThreadData.Running then
              begin
                StopCountThread;  {}// clear counters and set them
                ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnCount);
                Res := LRESULT(True);
              end
              else
              begin
                StartCountThread(FOwner.DlgHwnd, CountData); {}// clear counters and set them
                ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnAbort);
                Res := LRESULT(True);
              end;
          end;
      end;
  end;
end;

// ***** MAIN PLUGIN FUNCTIONS ***** \\

// initialize stuff with given PLUGINDATA members
procedure Init(var pd: TPLUGINDATA);
begin
  CurrLangId := PRIMARYLANGID(pd.wLangModule);

  dlgMain := TMainDlg.Create(pd);
  dlgMain.Persistent := True;

  //...
end;

// do main work here
procedure Execute(var pd: TPLUGINDATA);
begin
  ZeroMem(CountData, SizeOf(CountData));
  CountData.hMainWnd := pd.hMainWnd;
  CountData.hEditWnd := pd.hWndEdit;

  if not GetFileInfo(CountData, FileStats) then
  begin
    MsgBox(LangString(idMsgGetPropsFail), iStop);
    Exit;
  end;

  if dlgMain.ShowModal = -1 then
  begin
    MsgBox(LangString(idMsgShowDlgFail) + NL + LastErrMsg, iStop);
    Exit;
  end;
end;

// cleanup
procedure Finish;
begin
  DestroyIcon(FileStats.hIcon);
  FreeAndNil(dlgMain);
end;

// Identification
procedure DllAkelPadID(var pv: TPLUGINVERSION); cdecl;
begin
  pv.dwAkelDllVersion := AkelDLL;
  pv.dwExeMinVersion3x := MakeIdentifier(-1, -1, -1, -1);
  pv.dwExeMinVersion4x := MakeIdentifier(4, 7, 0, 0);
  pv.pPluginName := PAnsiChar(PluginName);
end;

// Plugin extern function
procedure Main(var pd: TPLUGINDATA); cdecl;
begin
  // Function doesn't support autoload
  pd.dwSupport := pd.dwSupport or PDS_NOAUTOLOAD;
  if (pd.dwSupport and PDS_GETSUPPORT) <> 0 then
    Exit;

  // Init stuff
  Init(pd);

  // Do main job here
  Execute(pd);

  // Cleanup
  Finish;

end;

// Entry point
procedure CustomDllProc(dwReason: DWORD);
begin
  case dwReason of
    DLL_PROCESS_ATTACH: ;
    DLL_PROCESS_DETACH: ;
    DLL_THREAD_ATTACH:  ;
    DLL_THREAD_DETACH:  ;
  end;
end;

exports
  DllAkelPadID,
  Main;

begin
  DllProc := @CustomDllProc;
  CustomDllProc(DLL_PROCESS_ATTACH);
end.
