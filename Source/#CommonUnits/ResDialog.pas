{****************************************************************************
  TResDialog - �����, ����������� ����� ��������� ������� �� �������.
               ����� ���������� �������� ��� ��������������� ���������� �
               ��������� ���������� ������/�������� � ����������� ���������,
               ������� ������������ WM_GET/SETTEXT, ������������ �������,
               ����������� �������� �� �������.
****************************************************************************}

unit ResDialog;

{$MESSAGE 'todo: }
// * ������ ��� �������� ���� ���������� (+����� ������������ - ������� ���� ������� )
// * �������� � ������� ����������� �������������� �������� - ��������.
// * add idyes, idno, ... (?) - or just post close message on custom close codes?
// * (?) �������� ����
// * �������� ����������� ��������� ��������� � ������ �������
// * �������� �� ���� - ?
// * (?) ������������ ��������� ��������� � dialogproc

interface

uses Windows, Messages, CommCtrl;

// ������-���������, ������� ������������ � ���������� OnDialogProc �����
// ��������, ����� ������������ � ����� ����������� ���� ��������������
const
  RDM_BASE       = WM_APP   + $F0;
  RDM_DLGOPENING = RDM_BASE + 1;
  RDM_DLGCLOSING = RDM_BASE + 2;
  RDM_DLGCLOSED  = RDM_BASE + 3;

type
  // See "Tooltip Styles" and uFlags member of the TOOLINFO structure in MSDN for description
  TTooltipStyle = (ttsAlwaysTip, ttsNoPrefix, ttsNoAnimate, ttsNoFade,
                   ttsBalloon, ttsClose, ttsUseVisualStyle,
                   ttfCenterTip, ttfRTLreading, ttfSubClass, ttfTrack,
                   ttfAbsolute, ttfTransparent, ttfParseLinks);

  TDlgItemData = record
    ID: Integer;
    Text: string;
    Tooltip: string;
  end;

  TResDialog = class
  strict protected
    type
      TDlgMode = (dmUndefined, dmModal, dmModeless);
    const
      CODE_TEXT = 0;
      CODE_TTIP = 1;
      TooltipStyleSet = [ttsAlwaysTip..ttsUseVisualStyle];
      TooltipFlagSet  = [ttfCenterTip..ttfParseLinks];
      TooltipStyleValues: array[TTooltipStyle] of Cardinal =
        (TTS_ALWAYSTIP, TTS_NOPREFIX, TTS_NOANIMATE, TTS_NOFADE, TTS_BALLOON, TTS_CLOSE, TTS_USEVISUALSTYLE,
         TTF_CENTERTIP, TTF_RTLREADING, TTF_SUBCLASS, TTF_TRACK, TTF_ABSOLUTE, TTF_TRANSPARENT, TTF_PARSELINKS);
    var
    fMode: TDlgMode;              // ������� ����� ������ ����
    fPersistent: Boolean;         // ���� True, ������ ����������� ������ �������� ����� LockResource
    fDlgResName: string;          // �������� ������� - ���������, ����� ������ �� ����������
    fDlgResID: LPTSTR;            // ������������� �������
    fDlgTemplate: PDlgTemplate;   // ��������� �� ����������� ������ (���� fPersistent = True)
    fBuf: Pointer;                // ����� ��� ��������� ������ ���������
    fBufSize: Cardinal;           // ������ ������
    fHInst: HINST;                // ����� ������, �� �������� ����������� ������
    fParentHwnd, fDlgHwnd: HWND;  // ������ ������������� � ����������� ����
    fTooltipHwnd: HWND;           // ����� ���� ��������
    fShowTooltips: Boolean;       // ���������� �� ������� ��� ��������� ! ����������� ������ ��� ������ ������� (���� ������ ��� ������, ������ �� ������)
    fCaption: string;             // ��������� ����
    fItemData: array of TDlgItemData;
    // ����� ������� �����������, ��������� - ������ ������ � ����
    constructor Create(const Instance: HINST; DlgResID: LPTSTR; ParentWnd: HWND); overload;
    // setters
    procedure SetCaption(val: string);
    procedure SetPersistent(val: Boolean);
    procedure SetParent(ParentHwnd: HWND);
    procedure SetItemText(ID: Integer; What: Integer; const Text: string);
    procedure SetItemVisible(ID: Integer; Visible: Boolean);
    // getters
    function GetItem(ID: Integer): HWND;
    function GetItemText(ID: Integer; What: Integer): string;
    function GetItemVisible(ID: Integer): Boolean;
    // ������
    procedure SetBufSize(Size: Cardinal);
    function Load: Boolean;
    function IndexOf(ID: Integer): Integer;
    procedure InternalSetItemTooltip(ID: Integer; const Tooltip: string);
    procedure InternalSetItemText(ID: Integer; const Text: string);
    function InternalGetItemText(ID: Integer): string;
    // �����������, ���������� � �������� ����� ��������� ���� � ������� ��� �� ������ �
    // ����� �������� � ������������ ��������������. ������� ������ ����������� �� ���
    // ���������� ����� �������� ��� ����� � ����������
    procedure DoBeforeShow; virtual;
    procedure DoBeforeClose(out Allow: Boolean; Code: Byte); virtual;
    procedure DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT); virtual;
  private // ��� ������� �� DefDialogProc
    // ������� ���������
    function DialogProc(wnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
  public
    // ������� ��� ������� �������. ����� �������������� ��� ����������� � ����� �
    // ���������� � ������ ����� ��������� ���� � ������� ��� �� ������ (��������, �����
    // ��������� �������� � ����������� ��������). ����������, ��������� �� ShowModal
    // ���� ������� �������� �� ����������
    OnBeforeShow: procedure(Sender: TResDialog) of object;
    // ������� ��� �������� �������. ����� �������������� ��� ����������� � ����� �
    // ���������� � ������ ����� �������� � ������������ ���� (��������, �����
    // ��������� �������� �� ����������� ���������). ����������,
    // ��������� ����� ShowModal ���� ������� �������� ������������.
    // ���������� Allow � False ����� ��������� �������� �������.
    OnBeforeClose: procedure(Sender: TResDialog; out Allow: Boolean; Code: Byte) of object;
    // ������� ������� �� ������� ���������, ������ ���������� Res � True, ���� ��������� ���������� {}
    //   RDM_DLGOPENING (������ WM_INITDIALOG) - ����� ��������� � ������� �������
    //   RDM_DLGCLOSING (������ WM_COMMAND � wParam �� EndDlgIDs) - ��� ��������
    //   RDM_DLGCLOSED  (������ WM_NCDESTROY) - ����� ��������
    OnDialogProc: procedure(Sender: TResDialog; msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT) of object;

    // ����� ��������������� ��������, ������� �� ������� ��������� ������
    // �������������� �� ������ ���� ������ 255!
    EndDlgIDs: set of Byte;
    // ����� ������� ��� �������� ���������� ���� ����� ������ ������
    Tag: NativeUInt;
    // ����� ��������    ! ����������� ������ ��� ������ ������� (���� ������ ��� ������, ������ �� ������)
    TooltipStyles: set of TTooltipStyle;

    constructor Create(const ResName: string; ParentWnd: HWND); overload;
    constructor Create(const ResID: Word;     ParentWnd: HWND); overload;
    constructor Create(const Instance: HINST; ResName: string; ParentWnd: HWND); overload;
    constructor Create(const Instance: HINST; ResID: Word;     ParentWnd: HWND); overload;
    destructor Destroy; override;

    procedure SetItemData(const ItemData: array of TDlgItemData);
    function Show(ShowCmd: Integer = SW_NORMAL): Boolean;
    function ShowModal: INT_PTR;
    procedure ProcessMessages;

    property DlgHwnd: HWND                 read fDlgHwnd;
    property ParentHwnd: HWND              read fParentHwnd    write SetParent;
    property TooltipHwnd: HWND             read fTooltipHwnd;
    property Caption: string               read fCaption       write SetCaption;
    property Persistent: Boolean           read fPersistent    write SetPersistent;
    property ShowTooltips: Boolean         read fShowTooltips  write fShowTooltips;

    property Item[ID: Integer]: HWND                          read GetItem;
    property ItemText[ID: Integer]: string    index CODE_TEXT read GetItemText write SetItemText;
    property ItemTooltip[ID: Integer]: string index CODE_TTIP read GetItemText write SetItemText;
    property ItemVisible[ID: Integer]: Boolean                read GetItemVisible write SetItemVisible;
  end;

function ItemData(ID: Integer; const Text: string; const Tooltip: string = ''): TDlgItemData; inline;

implementation

const
  PropName = 'Fr0sT_Dlg_HWnd';

// ������� ��������� �������
function DefDialogProc(wnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var dlg: TResDialog;
begin
  // ������������� - ������ ��������� �� ������ � �������� ����
  if msg = WM_INITDIALOG then
    SetProp(wnd, PropName, lParam);
  // ����������� ��������� �� ������
  dlg := TResDialog(Pointer(GetProp(wnd, PropName)));
  // ���� ���� �����������, ������� prop (!) ����� ����� dlg.DialogProc ����� �������� ��������� ���
  if msg = WM_NCDESTROY then
    RemoveProp(wnd, PropName);
  // �������� ����������
  if dlg = nil
    then Result := LRESULT(False)
    else Result := dlg.DialogProc(wnd, msg, wParam, lParam);
end;

// ��������������� �������, ����������� ��������� ������, ��������� ��� SetItemData,
// ��� �������������� ����������
function ItemData(ID: Integer; const Text: string; const Tooltip: string): TDlgItemData;
begin
  Result.ID := ID;
  Result.Text := Text;
  Result.Tooltip := Tooltip;
end;

{$REGION 'TResDialog'}

// create/destroy

constructor TResDialog.Create(const ResName: string; ParentWnd: HWND);
begin
  Create(HInstance, ResName, ParentWnd);
end;

constructor TResDialog.Create(const Instance: HINST; ResName: string; ParentWnd: HWND);
begin
  fDlgResName := ResName;
  Create(Instance, LPTSTR(fDlgResName), ParentWnd);
end;

constructor TResDialog.Create(const ResID: Word; ParentWnd: HWND);
begin
  Create(HInstance, ResID, ParentWnd);
end;

constructor TResDialog.Create(const Instance: HINST; ResID: Word; ParentWnd: HWND);
begin
  Create(Instance, MakeIntResource(ResID), ParentWnd);
end;

constructor TResDialog.Create(const Instance: HINST; DlgResID: LPTSTR; ParentWnd: HWND);
begin
  fHInst := Instance;
  fDlgResID := DlgResID;
  fParentHwnd := ParentWnd;
  EndDlgIDs := [IDOK, IDCANCEL];
end;

destructor TResDialog.Destroy;
begin
  SetBufSize(0); // release buffer
  inherited;
end;

// methods

procedure TResDialog.ProcessMessages;
var Msg: TMsg;
    MsgExists: Boolean;
begin
  repeat
    MsgExists := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
    // no messages in the queue
    if not MsgExists then Break;
    if not IsDialogMessage(fDlgHwnd, Msg) then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  until False;
end;

// ��������� ������ � �������� ��������� �� ���� (������ � ������ Persistent)
function TResDialog.Load: Boolean;
var hRes: HRSRC;
    hGl: HGLOBAL;
begin
  // ��� ���������
  if fDlgTemplate <> nil then
    Exit(True);
  // ��������� ������
  hRes := FindResource(fHInst, fDlgResID, RT_DIALOG);
  if hRes = 0 then Exit(False);
  hGl := LoadResource(fHInst, hRes);
  if hGl = 0 then Exit(False);
  fDlgTemplate := LockResource(hGl);
  if fDlgTemplate = nil then Exit(False);
  Result := True;
end;

// �������� � ����� ���� ������� � ��������� ������.
// ���������� 0 ��� -1 � ������ ������
function TResDialog.ShowModal: INT_PTR;
begin
  fMode := dmModal;
  if fPersistent then
  begin
    if not Load then Exit(-1);
    Result := DialogBoxIndirectParam(fHInst, fDlgTemplate^, fParentHwnd, @DefDialogProc, LPARAM(Self));
  end
  else
    Result := DialogBoxParam(fHInst, fDlgResID, fParentHwnd, @DefDialogProc, LPARAM(Self));
end;

// �������� � ����� ���� ������� � ����������� ������
function TResDialog.Show(ShowCmd: Integer): Boolean;
begin
  // ���� ���� �� ����������, ������ ���
  if fDlgHwnd = 0 then
  begin
    Result := False;
    if fPersistent then
    begin
      if not Load then Exit;
      fDlgHwnd := CreateDialogIndirectParam(fHInst, fDlgTemplate^, fParentHwnd, @DefDialogProc, LPARAM(Self));
    end
    else
      fDlgHwnd := CreateDialogParam(fHInst, fDlgResID, fParentHwnd, @DefDialogProc, LPARAM(Self));
    if fDlgHwnd = 0 then Exit;
    fMode := dmModeless;
  end;
  // ���������� ����
  ShowWindow(fDlgHwnd, ShowCmd); // ! �������� �� LastErr ��������� - � ��������� � tab control ��������
                                 // ��������� � ERROR_NOT_ENOUGH_MEMORY, ���� �� ��������.
  Result := True;
end;

// ���������� ������� ���������. ���������� ������� ����, ��� ��������� ����������.
// ���� ���������� False, ��������� ������������ ������������ �����������.
// ����� �� �������� ��������� ������������ DoDlgInit/Close - ���������
// ������ � � ����� ���������� OnDialogProc
function TResDialog.DialogProc(wnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var Allow: Boolean;
    i: Integer;
    Styles: UINT;
    style: TTooltipStyle;
begin
  // "Windows has this unfortunate habit of sending WM_COMMAND and WM_NOTIFY messages
  // before WM_INITDIALOG and after WM_DESTROY"
  if (fDlgHwnd = 0) and (msg <> WM_INITDIALOG) then
    Exit(LRESULT(False));

  case msg of
    // ������ ������, �������������
    WM_INITDIALOG:
      begin
        fDlgHwnd := wnd;
        // ������ ���������, ������ ���� �� ����� ���� � ���� ��� ��� ���������
        if (GetWindowTextLength(fDlgHwnd) = 0) or (fCaption <> '') then
          SetWindowText(fDlgHwnd, PChar(fCaption));
        // ������ ������� ���������
        for i := Low(fItemData) to High(fItemData) do
          InternalSetItemText(fItemData[i].ID, fItemData[i].Text);
        // ������� ���� �������� � ��������� ��������
        if fShowTooltips then
        begin
          Styles := 0; // ����������� ������ ���
          // ���������� �������������� �����, � ���� ��� �� ������ ������, ��������� � �������� ��������
          for style in TooltipStyles do
            if style in TooltipStyleSet then
              Styles := Styles or TooltipStyleValues[style];
          fTooltipHwnd := CreateWindowEx(0, TOOLTIPS_CLASS, nil, Styles,
                                         0, 0, 0, 0, fDlgHwnd, 0, HInstance, nil);
          SetWindowPos(fTooltipHwnd, HWND_TOPMOST, 0, 0, 0, 0,
                       SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
          // ��������� ��� �������� � ��������� ���������
          for i := Low(fItemData) to High(fItemData) do
            if fItemData[i].Tooltip <> '' then
              InternalSetItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
        end;
        DoBeforeShow;
        Result := LRESULT(True);
        DoDialogProc(RDM_DLGOPENING, wParam, lParam, Result);
        Exit;
      end;
    // ����� �����-�� ������� � �������, ���������, �� ������ ���������� �� ���.
    // ���� ��, ��������� ������
    WM_COMMAND:
      if LOWORD(wParam) in EndDlgIDs then
      begin
        Result := LRESULT(True);
        // ���� �� ������, �������� �������� ���������
        if LOWORD(wParam) <> IDCANCEL then
          for i := Low(fItemData) to High(fItemData) do
            fItemData[i].Text := InternalGetItemText(fItemData[i].ID);
        Allow := True;
        DoBeforeClose(Allow, LOWORD(wParam));
        if not Allow then Exit;
        DoDialogProc(RDM_DLGCLOSING, wParam, lParam, Result);
        case fMode of
          dmModal:    EndDialog(fDlgHwnd, LOWORD(wParam));
          dmModeless: DestroyWindow(fDlgHwnd);
        end;
        Exit;
      end;
    // ���� ����������, �������� ������
    WM_NCDESTROY:
      begin
        fMode := dmUndefined;
        DestroyWindow(fTooltipHwnd); // ���������� ���� ��������
        fTooltipHwnd := 0;
        fDlgHwnd := 0;
        fParentHwnd := 0; // �� ������ ������ �������� � ����� ������������� ����
        Result := LRESULT(False);
        DoDialogProc(RDM_DLGCLOSED, wParam, lParam, Result);
        Exit;
      end;
  end; // case
  // ��� ��������� ������ - �������������� ������ ������������� ��������� � ��������� ����������
  Result := LRESULT(False);
  DoDialogProc(msg, wParam, lParam, Result);
end;

// set/get item text/tooltip

// ����� � ������ itemdata �� ID
function TResDialog.IndexOf(ID: Integer): Integer;
begin
  for Result := Low(fItemData) to High(fItemData) do
    if fItemData[Result].ID = ID then
      Exit;
  Result := -1;
end;

// ��������� ������ � ������� Tooltip ��� �������� � ��������������� ID.
// ���� Tooltip - ������ ������, ������� ������� �� ������.
procedure TResDialog.InternalSetItemTooltip(ID: Integer; const Tooltip: string);
var ti: TToolInfo;
    Flags: UINT;
    flag: TTooltipStyle;
begin
  FillChar(ti, SizeOf(ti), 0);
  ti.cbSize := SizeOf(ti);
  ti.hwnd := fDlgHwnd;
  ti.uId := Item[ID];
  if Tooltip <> '' then
  begin
    Flags := TTF_IDISHWND or TTF_SUBCLASS; // ����������� �����
    // ���������� �������������� �����, � ���� ��� �� ������ ������, ��������� � �������� ��������
    for flag in TooltipStyles do
      if flag in TooltipFlagSet then
        Flags := Flags or TooltipStyleValues[flag];
    ti.uFlags := Flags;
    ti.lpszText := PChar(Tooltip);
    SendMessage(fTooltipHwnd, TTM_ADDTOOL, 0, LPARAM(@ti));
  end
  else
    SendMessage(fTooltipHwnd, TTM_DELTOOL, 0, LPARAM(@ti));
end;

// ������� ��� ��������� ������ �� �������� �������
//                        !!! �������� !!!
// CharLen := GetWindowTextLength(hEdit) �������� ��� ����, ����� ��������
// DateTimePicker, � �������� ���������� ��������� ����� (��� ��� �������).
// GetWindowTextLength(hEdit) + 1 �������� � ����, ��� �������� � �����������
// ������� ������, ������� ������ ������������ SetString(CharLen). ��� ���������������
// ������� ���: ����� ���������� �������� ������ �� 1 ������, � ��� ������������
// ������������ PChar, ������� ���� ��� ������� �� �������� ������������.
function TResDialog.InternalGetItemText(ID: Integer): string;
var CharLen: Cardinal;
    pDest: PChar;
begin
  // ���������� ����� ������ � �������� (� ��������!!) (+����������� �������!!)
  CharLen := GetWindowTextLength(Item[ID]) + 1;
  // ��������� ����� �� ������ � � ����� ������ ��������� ���� ����� ������
  SetBufSize(CharLen*SizeOf(Char));
  pDest := PChar(fBuf);
  GetDlgItemText(fDlgHwnd, ID, pDest, CharLen);
  (pDest+CharLen-1)^ := #0; // ����� �� ���������!!
  Result := pDest;
end;

// ��������� ��� ��������� ������ ��������
procedure TResDialog.InternalSetItemText(ID: Integer; const Text: string);
begin
  SetDlgItemText(fDlgHwnd, ID, PChar(Text));
end;

// ���������� ��������� ����; ���� ������ �������, ����� � ������ ���
procedure TResDialog.SetCaption(val: string);
begin
  if fCaption = val then Exit;
  fCaption := val;
  if fDlgHwnd <> 0 then
    SetWindowText(fDlgHwnd, PChar(fCaption));
end;

// ��������� ������ ��������, ��������� ����� �������������.
// ���� ������ ������, ������ �������� ����� �� ��������.
function TResDialog.GetItemText(ID: Integer; What: Integer): string;
var idx: Integer;
begin
  idx := IndexOf(ID);
  if idx <> -1 then
  begin
    if fDlgHwnd <> 0 then
      fItemData[idx].Text := InternalGetItemText(fItemData[idx].ID);
    Result := fItemData[idx].Text;
  end
  else
    Result := '';
end;

// ��������� ������ ��������, ��������� ����� �������������.
// ���� ������ ������, ����� ������ ����� �� ��������.
// ���� � ������� ������ � ����� ID ���, ��������� ����� �������.
procedure TResDialog.SetItemText(ID: Integer; What: Integer; const Text: string);
var idx: Integer;
begin
  idx := IndexOf(ID);
  // �������� � ��������� ID � ������� ��� - ���������
  if idx = -1 then
  begin
    idx := Length(fItemData);
    SetLength(fItemData, idx + 1);
    fItemData[idx].ID := ID;
  end;
  // ����������� ��������
  case What of
    CODE_TEXT:
      begin
        fItemData[idx].Text := Text;
        if fDlgHwnd <> 0 then
          InternalSetItemText(fItemData[idx].ID, fItemData[idx].Text);
      end;
    CODE_TTIP:
      begin
        fItemData[idx].Tooltip := Text;
        if fTooltipHwnd <> 0 then
          InternalSetItemTooltip(fItemData[idx].ID, fItemData[idx].Tooltip);
      end;
  end;
end;

// ��������� ������� ����� ���������� ���������, �������� ����� �������������.
// ��������� ������ �������� TDlgItemData. ���� ������ ������, ����� ������ ����� �� ��������
// ! �������� ��������� ������ ! ����� �� ���������� � ��������, �������� ������
// ���� ���� �������
// ! � ���� �� ������ ����� ���������� ��� �������� � ��������� ��� �������� �������
// ! ������ array of TDlgItemData - ����� �� ���� ��������� ��������� ������
procedure TResDialog.SetItemData(const ItemData: array of TDlgItemData);
var i: Integer;
begin
  // ������ ����� ���� �������
  if fDlgHwnd <> 0 then Exit;
  SetLength(fItemData, 0); // ������� ��������� ��������
  SetLength(fItemData, Length(ItemData));
  for i := Low(ItemData) to High(ItemData) do
  begin
    fItemData[i] := ItemData[i];
    InternalSetItemText(fItemData[i].ID, fItemData[i].Text);
    // ���� ���� �������� ���������� - �������� �������. ����������� ���� ���
    // ������ ��������, ����� ����� ����������� �������� ������.
    if fTooltipHwnd <> 0 then
      InternalSetItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
  end;
end;

// �����/������� ��������
procedure TResDialog.SetItemVisible(ID: Integer; Visible: Boolean);
begin
  if Visible
    then ShowWindow(Item[ID], SW_SHOW)
    else ShowWindow(Item[ID], SW_HIDE);
end;

// ����������� ��������� ��������
function TResDialog.GetItemVisible(ID: Integer): Boolean;
begin
  Result := IsWindowVisible(Item[ID]);
end;

// ����� ������������� ����
procedure TResDialog.SetParent(ParentHwnd: HWND);
begin
  if fParentHwnd = ParentHwnd then Exit;
  fParentHwnd := ParentHwnd;
  // ���� ������� ������� - ������ �������� � ����
  if fDlgHwnd <> 0 then
    Windows.SetParent(fDlgHwnd, fParentHwnd);
end;

// ��������� ����� ������������
procedure TResDialog.SetPersistent(val: Boolean);
begin
  if not val then
    fDlgTemplate := nil;
  fPersistent := val;
end;

// ��������� ������ ������� ��� ������ + ���������� ��� ������ � ����� ������
procedure TResDialog.SetBufSize(Size: Cardinal);
begin
  // ����� �� ������ �������� �������� � �������, ������� �� ���
  // Size = 0 - ������ ������, ������� ���������� �����
  if (fBufSize >= Size) and (Size <> 0) then
  begin
    FillChar(fBuf^, fBufSize, 0);
    Exit;
  end;
  // ����������� ������ ���������� ������
  if (fBufSize <> 0) and (fBuf <> nil) then
  begin
    FreeMem(fBuf); fBuf := nil; fBufSize := 0;
  end;
  // ������������ ������ � ��������� ������, �� ������ ������
  if Size > 0 then
  begin
    fBufSize := Size;
    GetMem(fBuf, fBufSize);
    FillChar(fBuf^, fBufSize, 0);
  end;
end;

// Get window handle of a dialog item by ID. Just a simple wrapper to make DlgItem property
function TResDialog.GetItem(ID: Integer): HWND;
begin
  Result := GetDlgItem(fDlgHwnd, ID);
end;

// Stubs that launch event handlers

procedure TResDialog.DoBeforeShow;
begin
  if Assigned(OnBeforeShow) then OnBeforeShow(Self);
end;

procedure TResDialog.DoBeforeClose(out Allow: Boolean; Code: Byte);
begin
  if Assigned(OnBeforeClose) then OnBeforeClose(Self, Allow, Code);
end;

procedure TResDialog.DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
begin
  if Assigned(OnDialogProc) then OnDialogProc(Self, msg, wParam, lParam, Res);
end;

{$ENDREGION}

end.
