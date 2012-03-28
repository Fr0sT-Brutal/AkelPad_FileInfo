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
      TDlgMode = (dmUndefined, dmModal, dmModalCustom, dmModeless);
    const
      CODE_TEXT = 0;
      CODE_TTIP = 1;
      TooltipStyleSet = [ttsAlwaysTip..ttsUseVisualStyle];
      TooltipFlagSet  = [ttfCenterTip..ttfParseLinks];
      TooltipStyleValues: array[TTooltipStyle] of Cardinal =
        (TTS_ALWAYSTIP, TTS_NOPREFIX, TTS_NOANIMATE, TTS_NOFADE, TTS_BALLOON, TTS_CLOSE, TTS_USEVISUALSTYLE,
         TTF_CENTERTIP, TTF_RTLREADING, TTF_SUBCLASS, TTF_TRACK, TTF_ABSOLUTE, TTF_TRANSPARENT, TTF_PARSELINKS);
      StandardEndIDs: array[0..5] of Integer =
        (IDOK, IDCANCEL, IDABORT, IDYES, IDNO, IDCLOSE);
    var
    fMode: TDlgMode;              // ������� ����� ������ ����
    fPersistent: Boolean;         // ���� True, ������ ����������� ������ �������� ����� LockResource
    fDlgResName: string;          // �������� ������� - ���������, ����� ������ �� ����������
    fDlgResID: LPTSTR;            // ������������� �������
    fDlgTemplate: PDlgTemplate;   // ��������� �� ����������� ������ (���� fPersistent = True)
    fBuf: Pointer;                // ����� ��� ��������� ������ ���������
    fBufSize: Cardinal;           // ������ ������
    fModalResult: INT_PTR;        // ��������� ID ��������, ���������� ������, ��� ������� dmModalCustom � dmModeless
    fInst: HINST;                 // ����� ������, �� �������� ����������� ������
    fParentWnd, fDlgWnd: HWND;    // ������ ������������� � ����������� ����
    fTooltipWnd: HWND;            // ����� ���� ��������
    fShowTooltips: Boolean;       // ���������� �� ������� ��� ��������� ! ����������� ������ ��� ������ ������� (���� ������ ��� ������, ������ �� ������)
    fCaption: string;             // ��������� ����
    fItemData: array of TDlgItemData;
    fEndDlgBtnIDs: array of Integer; // ����� ��������������� ������, ������� �� ������� ��������� ������
                                     // �������� ��������� ����������� ���������������:
                                     // IDOK, IDCANCEL, IDABORT, IDYES, IDNO, IDCLOSE
    // ����� ������� �����������, ��������� - ������ ������ � ����
    constructor Create(const Instance: HINST; DlgResID: LPTSTR; Parent: HWND); overload;
    // setters
    procedure SetCaption(val: string);
    procedure SetPersistent(val: Boolean);
    procedure SetParent(Parent: HWND);
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
    procedure DoDialogProc(const Msg: TMsg; out Res: LRESULT); virtual;
    procedure DoBeforeMessage(const Msg: TMsg; out Res: LRESULT; out Handled: Boolean); virtual;
  private // ��� ������� �� DefDialogProc
    // ������� ���������
    function DialogProc(const Msg: TMsg): LRESULT;
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
    // ������ ��������� ���������, �������� ����� ���������� ���������:
    //   RDM_DLGOPENING (������ WM_INITDIALOG) - ����� ��������� � ������� �������
    //   RDM_DLGCLOSING (������ WM_COMMAND � wParam �� EndDlgIDs) - ��� ��������
    //   RDM_DLGCLOSED  (������ WM_NCDESTROY) - ����� ��������
    OnDialogProc: procedure(Sender: TResDialog; const Msg: TMsg; out Res: LRESULT) of object;
    // ���� ���������� ���� ����������� ���������.
    // !! ���������� ������ ���� ������ ������� ����� ������ Show ��� ShowModalCustom.
    // ��� ���� ����������� ����������� ���� ������� ��������� (��� ������ Show ��� ����
    // ��������� ������� ����� ����� ProcessMessages), ������� ��������� ����������� ��
    // ����� ����������� ��������� �� ��������� ��� �������� IsDialogMessage. ������ ����
    // �������, ��� � ���� ���������� ����� ���������� ��� ��������� �� �������, �������
    // �� ����������� � ����������� ����. ������� ���������� ������ ��������� ������� (Msg.hwnd)
    // ���������, ����� �� ������ ����� ���������! (����� ����� if GetDlgCtrlID(Msg.hwnd) = ...)
    // ���������� ����� ���������� Handled � True, ����� ���������� ��������� ������� ���������
    OnBeforeMessage: procedure(Sender: TResDialog; const Msg: TMsg; out Res: LRESULT; out Handled: Boolean) of object;

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
    function ShowModalCustom: INT_PTR;
    procedure ProcessMessages;
    procedure SetEndDlgIDs(IDs: array of Integer);

    property DlgWnd: HWND          read fDlgWnd;
    property ModalResult: INT_PTR  read fModalResult;
    property Parent: HWND          read fParentWnd    write SetParent;
    property TooltipWnd: HWND      read fTooltipWnd;
    property Caption: string       read fCaption       write SetCaption;
    property Persistent: Boolean   read fPersistent    write SetPersistent;
    property ShowTooltips: Boolean read fShowTooltips  write fShowTooltips;

    property Item[ID: Integer]: HWND                          read GetItem;
    property ItemText[ID: Integer]: string    index CODE_TEXT read GetItemText write SetItemText;
    property ItemTooltip[ID: Integer]: string index CODE_TTIP read GetItemText write SetItemText;
    property ItemVisible[ID: Integer]: Boolean                read GetItemVisible write SetItemVisible;
  end;

function ItemData(ID: Integer; const Text: string; const Tooltip: string = ''): TDlgItemData; inline;

// *** ������� ������ WinAPI ������� ***

type
  TShiftKey = (ssShift, ssShiftLeft, ssShiftRight,
               ssAlt, ssAltLeft, ssAltRight,
               ssCtrl, ssCtrlLeft, ssCtrlRight,
               ssWin, ssWinLeft, ssWinRight);
  TShiftState = set of TShiftKey;

function KeyPressed(KeyState: Byte): Boolean; inline;
function GetKeyboardShiftState(DifferRightLeft: Boolean = False): TShiftState; inline;
function KeyboardShiftState(KeybState: TKeyboardState; DifferRightLeft: Boolean = False): TShiftState;

function MsgStruct(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): TMsg;

type
  TSubclassWndProc = procedure(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT; out Handled: Boolean);

function SubclassControl(hWnd: HWND; NewWndProc: TSubclassWndProc): Boolean;

implementation

// *** ��������� ������ ���������� ***

// ��������, ������ �� �������
function KeyPressed(KeyState: Byte): Boolean;
begin
  Result := KeyState and $80 <> 0;
end;

// �������� ��������� ������� ������������� �� ������� ��������� ������.
// DifferRightLeft - ��������� ����� � ������.
function KeyboardShiftState(KeybState: TKeyboardState; DifferRightLeft: Boolean = False): TShiftState;
begin
  Result := [];

  if KeyPressed(KeybState[VK_CONTROL]) then Include(Result, ssCtrl);
  if DifferRightLeft then
  begin
    if KeyPressed(KeybState[VK_LCONTROL]) then Include(Result, ssCtrlLeft);
    if KeyPressed(KeybState[VK_RCONTROL]) then Include(Result, ssCtrlRight);
  end;

  if KeyPressed(KeybState[VK_MENU]) then Include(Result, ssAlt);
  if DifferRightLeft then
  begin
    if KeyPressed(KeybState[VK_LMENU]) then Include(Result, ssAltLeft);
    if KeyPressed(KeybState[VK_RMENU]) then Include(Result, ssAltRight);
  end;

  if KeyPressed(KeybState[VK_SHIFT]) then Include(Result, ssShift);
  if DifferRightLeft then
  begin
    if KeyPressed(KeybState[VK_LSHIFT]) then Include(Result, ssShiftLeft);
    if KeyPressed(KeybState[VK_RSHIFT]) then Include(Result, ssShiftRight);
  end;

  if DifferRightLeft then
  begin
    if KeyPressed(KeybState[VK_LWIN]) then Include(Result, ssWinLeft);
    if KeyPressed(KeybState[VK_RWIN]) then Include(Result, ssWinRight);
  end
  else
    if KeyPressed(KeybState[VK_LWIN]) or KeyPressed(KeybState[VK_RWIN]) then Include(Result, ssWin);
end;

// �������� ������� ��������� ������� �������������.
// DifferRightLeft - ��������� ����� � ������.
function GetKeyboardShiftState(DifferRightLeft: Boolean = False): TShiftState;
var KeybState: TKeyboardState;
begin
  if not GetKeyboardState(KeybState)
    then Result := []
    else Result := KeyboardShiftState(KeybState, DifferRightLeft);
end;

// *** ����������� ��������. ��������� �� ������ ��������� �������� � �������� ***

const
  SubclassProp = 'Fr0sT_Subcl_WndProc';

type
  TSubclassData = record
    NewWndProc: TSubclassWndProc;
    OldWndProc: TFNWndProc;
  end;
  PSubclassData = ^TSubclassData;

// ���������-������, ������� ����� ���������� ��� ���� ���������
function SubclassWndProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  pSubclData: PSubclassData;
  Processed: Boolean;
begin
  Result := 0;
  pSubclData := PSubclassData(GetProp(hWnd, SubclassProp));
  if pSubclData = nil then Exit;

  // �������� ��������� ����������, ���� �� �� ��������� - �� ����������� ������� ���������
  Processed := False;
  pSubclData.NewWndProc(hWnd, msg, wParam, lParam, Result, Processed);
  if not Processed then
    Result := CallWindowProc(pSubclData.OldWndProc, hWnd, msg, wParam, lParam);

  // ���� ���� ������������ - ������� �������� � ���������� ������, ���������� ��� ������
  if msg = WM_NCDESTROY then
  begin
    RemoveProp(hWnd, SubclassProp);
    if pSubclData <> nil then FreeMem(pSubclData);
  end;
end;

// ������ ������� ��������� � �������� �� �����������
function SubclassControl(hWnd: HWND; NewWndProc: TSubclassWndProc): Boolean;
var
  pSubclData: PSubclassData;
  err: Cardinal;
begin
  Result := False;

  // ���������, �� ����������� �� ��� ���� �������
  pSubclData := PSubclassData(GetProp(hWnd, SubclassProp));
  if pSubclData <> nil then Exit;

  pSubclData := AllocMem(SizeOf(TSubclassData));
  // ������� �������� ��� ������ � ��������, ���� �� ��������� - �� �����
  if not SetProp(hWnd, SubclassProp, THandle(pSubclData)) then
  begin
    FreeMem(pSubclData);
    Exit;
  end;

  // ������ ������� ��������� � ��������, ���� �� ��������� - �� �����
  pSubclData.NewWndProc := NewWndProc;
  pSubclData.OldWndProc := TFNWndProc(SetWindowLongPtr(hWnd, GWLP_WNDPROC, LONG_PTR(@SubclassWndProc)));
  if pSubclData.OldWndProc = nil then
  begin
    err := GetLastError;
    RemoveProp(hWnd, SubclassProp);
    SetLastError(err);
    FreeMem(pSubclData);
    Exit;
  end;

  Result := True;
end;


function MsgStruct(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): TMsg;
begin
  ZeroMemory(@Result, SizeOf(@Result));
  Result.hwnd := hWnd;
  Result.message := msg;
  Result.wParam := wParam;
  Result.lParam := lParam;
end;

// *** ������� ***

const
  DlgProp = 'Fr0sT_Dlg_HWnd';

// ������� ��������� �������
function DefDialogProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var dlg: TResDialog;
begin
  // ������������� - ������ ��������� �� ������ � �������� ����
  if msg = WM_INITDIALOG then
    SetProp(hWnd, DlgProp, lParam);
  // ����������� ��������� �� ������
  dlg := TResDialog(Pointer(GetProp(hWnd, DlgProp)));
  // ���� ���� �����������, ������� prop (!) ����� ����� dlg.DialogProc ����� �������� ��������� ���
  if msg = WM_NCDESTROY then
    RemoveProp(hWnd, DlgProp);
  // �������� ����������
  if dlg = nil
    then Result := LRESULT(False)
    else Result := dlg.DialogProc(MsgStruct(hWnd, msg, wParam, lParam));
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

constructor TResDialog.Create(const Instance: HINST; DlgResID: LPTSTR; Parent: HWND);
begin
  fInst := Instance;
  fDlgResID := DlgResID;
  fParentWnd := Parent;
  SetEndDlgIDs([]); // set standard dialog ending IDs
end;

destructor TResDialog.Destroy;
begin
  SetBufSize(0); // release buffer
  inherited;
end;

// methods

// ����������� ���� ������� ���������, ��� ����������� �������� ���� ��� �������� �����������.
// ������������ ����� � �� ����������� � ������ ���� ���������, ��� ��� ���� ����������
// ����������� �� ���������, ���� "������������" ���������� ���� ����� EnableWindow
// (� ������ ShowModalCustom ��� ��� �������)
procedure TResDialog.ProcessMessages;
var Msg: TMsg;
    Handled: Boolean;
    Res: LRESULT;
begin
  repeat
    // ���� ���� ��������� - ���������� ���������� ���������� ������������
    if not IsWindow(fDlgWnd) then Break;

    case Integer(GetMessage(Msg, 0, 0, 0)) of
       0: Break;
      -1: ;
      else
      begin
        // ����������� ���������, ���� ���������� - ���� � ���������� ���������
        Handled := False;
        DoBeforeMessage(Msg, Res, Handled);
        if Handled then Continue;
        // ����������� ��������� ���������� ���������
        if IsDialogMessage(fDlgWnd, Msg) then Continue;
        // ��� ��������� ��������� - �������� �������
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
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
  hRes := FindResource(fInst, fDlgResID, RT_DIALOG);
  if hRes = 0 then Exit(False);
  hGl := LoadResource(fInst, hRes);
  if hGl = 0 then Exit(False);
  fDlgTemplate := LockResource(hGl);
  if fDlgTemplate = nil then Exit(False);
  Result := True;
end;

// �������� � ����� ���� ������� � ��������� ������.
// ���������� 0 ��� -1 � ������ ������
function TResDialog.ShowModal: INT_PTR;
begin
  // ���� �� ������ ������������
  if fDlgWnd <> 0 then Exit(-1);

  fModalResult := -1;
  if not IsWindow(fParentWnd) then fParentWnd := 0;

  fMode := dmModal; // ��������������
  if fPersistent then
    if Load then
      fModalResult := DialogBoxIndirectParam(fInst, fDlgTemplate^, fParentWnd, @DefDialogProc, LPARAM(Self))
    else
  else
    fModalResult := DialogBoxParam(fInst, fDlgResID, fParentwnd, @DefDialogProc, LPARAM(Self));

  if fModalResult <= 0 then
    fMode := dmUndefined;

  Result := fModalResult;
end;

// �������� � ����� ���� ������� � ����������� ������
function TResDialog.Show(ShowCmd: Integer): Boolean;
begin
  fModalResult := -1;
  if not IsWindow(fParentWnd) then fParentWnd := 0;

  // ���� ���� �� ����������, ������ ���
  if fDlgWnd = 0 then
  begin
    if fPersistent then
      if Load then
        fDlgWnd := CreateDialogIndirectParam(fInst, fDlgTemplate^, fParentWnd, @DefDialogProc, LPARAM(Self))
      else
    else
      fDlgWnd := CreateDialogParam(fInst, fDlgResID, fParentWnd, @DefDialogProc, LPARAM(Self));
  end;

  // ���� ���� ������� ������� - �������� ���
  if fDlgWnd <> 0 then
  begin
    fMode := dmModeless;
    ShowWindow(fDlgWnd, ShowCmd); // ! �������� �� LastErr ��������� - � ��������� � tab control ��������
                                   // ��������� � ERROR_NOT_ENOUGH_MEMORY, ���� �� ��������.
  end;

  Result := (fDlgWnd <> 0);
end;

// �������� � ����� ���� ������� � "����������� ��������" ���������� ������.
function TResDialog.ShowModalCustom: INT_PTR;
begin
  fModalResult := -1;
  if not IsWindow(fParentWnd) then fParentWnd := 0;

  // ���� ���� �� ����������, ������ ���
  if fDlgWnd = 0 then
  begin
    if fPersistent then
      if Load then
        fDlgWnd := CreateDialogIndirectParam(fInst, fDlgTemplate^, fParentWnd, @DefDialogProc, LPARAM(Self))
      else
    else
      fDlgWnd := CreateDialogParam(fInst, fDlgResID, fParentWnd, @DefDialogProc, LPARAM(Self));
  end;

  // ���� ���� ������� ������� - �������� � ��������� ���� ������� ���������
  if fDlgWnd <> 0 then
  begin
    fMode := dmModalCustom;
    EnableWindow(fParentWnd, False);  // ������ ��� ����������� �����������
    ShowWindow(fDlgWnd, SW_SHOW);
    ProcessMessages;
    EnableWindow(fParentWnd, True);
  end;

  Result := fModalResult;
end;

// ���������� ������� ���������. ���������� ������� ����, ��� ��������� ����������.
// ���� ���������� False, ��������� ������������ ������������ �����������.
// ����� �� �������� ��������� ������������ DoDlgInit/Close - ���������
// ������ � � ����� ���������� OnDialogProc
function TResDialog.DialogProc(const Msg: TMsg): LRESULT;
var Allow, EndDlgCmd: Boolean;
    i: Integer;
    Styles: UINT;
    style: TTooltipStyle;
begin
  // "Windows has this unfortunate habit of sending WM_COMMAND and WM_NOTIFY messages
  // before WM_INITDIALOG and after WM_DESTROY"
  if (fDlgWnd = 0) and (Msg.message <> WM_INITDIALOG) then
    Exit(LRESULT(False));

  case Msg.message of
    // ������ ������, �������������
    WM_INITDIALOG:
      begin
        fDlgWnd := Msg.hWnd;
        // ������ ���������, ������ ���� �� ����� ���� � ���� ��� ��� ���������
        if (GetWindowTextLength(fDlgWnd) = 0) or (fCaption <> '') then
          SetWindowText(fDlgWnd, PChar(fCaption));
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
          fTooltipWnd := CreateWindowEx(0, TOOLTIPS_CLASS, nil, Styles,
                                         0, 0, 0, 0, fDlgWnd, 0, HInstance, nil);
          SetWindowPos(fTooltipWnd, HWND_TOPMOST, 0, 0, 0, 0,
                       SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
          // ��������� ��� �������� � ��������� ���������
          for i := Low(fItemData) to High(fItemData) do
            if fItemData[i].Tooltip <> '' then
              InternalSetItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
        end;
        DoBeforeShow;
        Result := LRESULT(True);
        DoDialogProc(MsgStruct(fDlgWnd, RDM_DLGOPENING, Msg.wParam, Msg.lParam), Result);
        Exit;
      end; // WM_INITDIALOG

    // ������� �� ��������� ��������. ���������, ���� ��� ������ ������ ���� Escape, �� ��������� ������.
    WM_COMMAND:
      begin
        EndDlgCmd := False;
        // ����� Escape? HIWORD(wParam) = 0, LOWORD(wParam) = IDCANCEL
        if (HIWORD(Msg.wParam) = 0) and (LOWORD(Msg.wParam) = IDCANCEL) then
          EndDlgCmd := True
        // ������ ������? HIWORD(wParam) = BN_CLICKED, LOWORD(wParam) = Btn_ID
        else if HIWORD(Msg.wParam) = BN_CLICKED then
        begin
          for i in fEndDlgBtnIDs do // ���� ID ������ � ������ ID, ����������� ������
            if i = LOWORD(Msg.wParam) then
            begin
              EndDlgCmd := True;
              Break;
            end;
        end;
        // ������ ������������� �����������
        if EndDlgCmd then
        begin
          Result := LRESULT(True);
          // ���� �� ������, ��������� ������ ���������
          if LOWORD(Msg.wParam) <> IDCANCEL then
            for i := Low(fItemData) to High(fItemData) do
              fItemData[i].Text := InternalGetItemText(fItemData[i].ID);
          Allow := True;
          DoBeforeClose(Allow, LOWORD(Msg.wParam));
          if not Allow then Exit;
          DoDialogProc(MsgStruct(fDlgWnd, RDM_DLGCLOSING, Msg.wParam, Msg.lParam), Result);
          case fMode of
            dmModal:
              EndDialog(fDlgWnd, LOWORD(Msg.wParam));
            dmModeless, dmModalCustom:
              begin
                fModalResult := LOWORD(Msg.wParam);
                DestroyWindow(fDlgWnd);
              end;
          end;
          Exit;
        end;
      end; // WM_COMMAND

    // ���� ����������, �������� ������
    WM_NCDESTROY:
      begin
        fMode := dmUndefined;
        DestroyWindow(fTooltipWnd); // ���������� ���� ��������
        fTooltipWnd := 0;
        fDlgWnd := 0;
        Result := LRESULT(False);
        DoDialogProc(MsgStruct(fDlgWnd, RDM_DLGCLOSED, Msg.wParam, Msg.lParam), Result);
        Exit;
      end; // WM_NCDESTROY
  end; // case

  // ��� ��������� ������ - �������������� ������ ������������� ��������� � ��������� ����������
  Result := LRESULT(False);
  DoDialogProc(Msg, Result);
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
  ti.hwnd := fDlgWnd;
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
    SendMessage(fTooltipWnd, TTM_ADDTOOL, 0, LPARAM(@ti));
  end
  else
    SendMessage(fTooltipWnd, TTM_DELTOOL, 0, LPARAM(@ti));
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
  GetDlgItemText(fDlgWnd, ID, pDest, CharLen);
  (pDest+CharLen-1)^ := #0; // ����� �� ���������!!
  Result := pDest;
end;

// ��������� ��� ��������� ������ ��������
procedure TResDialog.InternalSetItemText(ID: Integer; const Text: string);
begin
  SetDlgItemText(fDlgWnd, ID, PChar(Text));
end;

// ���������� ��������� ����; ���� ������ �������, ����� � ������ ���
procedure TResDialog.SetCaption(val: string);
begin
  if fCaption = val then Exit;
  fCaption := val;
  if fDlgWnd <> 0 then
    SetWindowText(fDlgWnd, PChar(fCaption));
end;

// ��������� ������ ��������, ��������� ����� �������������.
// ���� ������ ������, ������ �������� ����� �� ��������.
// ���� � ������� ������ � ����� ID ���, ��������� ����� �������.
function TResDialog.GetItemText(ID: Integer; What: Integer): string;
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
  if fDlgWnd <> 0 then
    fItemData[idx].Text := InternalGetItemText(fItemData[idx].ID);
  Result := fItemData[idx].Text;
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
        if fDlgWnd <> 0 then
          InternalSetItemText(fItemData[idx].ID, fItemData[idx].Text);
      end;
    CODE_TTIP:
      begin
        fItemData[idx].Tooltip := Text;
        if fTooltipWnd <> 0 then
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
  if fDlgWnd <> 0 then Exit;
  SetLength(fItemData, 0); // ������� ��������� ��������
  SetLength(fItemData, Length(ItemData));
  for i := Low(ItemData) to High(ItemData) do
  begin
    fItemData[i] := ItemData[i];
    InternalSetItemText(fItemData[i].ID, fItemData[i].Text);
    // ���� ���� �������� ���������� - �������� �������. ����������� ���� ���
    // ������ ��������, ����� ����� ����������� �������� ������.
    if fTooltipWnd <> 0 then
      InternalSetItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
  end;
end;

// ����������� ����� ID, ����������� ������. ������ ������� ��������� �����������
// ����� �� StandardEndIDs. ����� �������� ������ ������, ����� � ������ �����
// ������ ����������� (��� ����������� � ������������).
procedure TResDialog.SetEndDlgIDs(IDs: array of Integer);
var i: Integer;
begin
  SetLength(fEndDlgBtnIDs, 0);
  SetLength(fEndDlgBtnIDs, Length(StandardEndIDs) + Length(IDs));
  for i := Low(StandardEndIDs) to High(StandardEndIDs) do
    fEndDlgBtnIDs[i] := StandardEndIDs[i];
  for i := Low(IDs) to High(IDs) do
    fEndDlgBtnIDs[Length(StandardEndIDs) + i] := IDs[i];
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
procedure TResDialog.SetParent(Parent: HWND);
begin
  if fParentWnd = Parent then Exit;
  fParentWnd := Parent;
  // ���� ������� ������� - ������ �������� � ����
  if fDlgWnd <> 0 then
    Windows.SetParent(fDlgWnd, fParentWnd);
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
  Result := GetDlgItem(fDlgWnd, ID);
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

procedure TResDialog.DoDialogProc(const Msg: TMsg; out Res: LRESULT);
begin
  if Assigned(OnDialogProc) then OnDialogProc(Self, Msg, Res);
end;

procedure TResDialog.DoBeforeMessage(const Msg: TMsg; out Res: LRESULT; out Handled: Boolean);
begin
  if Assigned(OnBeforeMessage) then OnBeforeMessage(Self, Msg, Res, Handled);
end;

{$ENDREGION}

end.
