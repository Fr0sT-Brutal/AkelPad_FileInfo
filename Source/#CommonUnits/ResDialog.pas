{****************************************************************************
  TResDialog - класс, позволяющий легко загружать диалоги из ресурса.
               Имеет встроенные средства для автоматического присвоения и
               получения введенного текста/надписей в большинстве контролов,
               которые поддерживают WM_GET/SETTEXT, поддерживает тултипы,
               однократную загрузку из ресурса.
****************************************************************************}

unit ResDialog;

{$MESSAGE 'todo: }
// * диалог как основное окно приложения (+показ немодального - крутить цикл выборки )
// * встроить в коллбэк возможность препятствовать закрытию - подумать.
// * (?) исчезает хинт
// * добавить возможность указывать заголовок и иконку баллуна
// * коллбэки на хинт - ?
// * (?) переправлять сообщения элементам в dialogproc

interface

uses Windows, Messages, CommCtrl;

// псевдо-сообщения, которые отправляются в обработчик OnDialogProc после
// создания, перед уничтожением и после уничтожения окна соответственно
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
    fMode: TDlgMode;              // текущий режим показа окна
    fPersistent: Boolean;         // если True, диалог загружается только единожды через LockResource
    fDlgResName: string;          // название ресурса - сохраняем, чтобы строка не потерялась
    fDlgResID: LPTSTR;            // идентификатор ресурса
    fDlgTemplate: PDlgTemplate;   // указатель на загруженный ресурс (если fPersistent = True)
    fBuf: Pointer;                // буфер для получения текста контролов
    fBufSize: Cardinal;           // размер буфера
    fModalResult: INT_PTR;        // сохраняет ID контрола, закрывшего диалог, для режимов dmModalCustom и dmModeless
    fInst: HINST;                 // хэндл модуля, из которого загружается ресурс
    fParentWnd, fDlgWnd: HWND;    // хэндлы родительского и диалогового окон
    fTooltipWnd: HWND;            // хэндл окна тултипов
    fShowTooltips: Boolean;       // показывать ли тултипы для контролов ! Срабатывает только при показе диалога (если диалог уже открыт, ничего не меняет)
    fCaption: string;             // заголовок окна
    fItemData: array of TDlgItemData;
    fEndDlgBtnIDs: array of Integer; // набор идентификаторов кнопок, нажатие на которые завершает диалог
                                     // Встроена поддержка стандартных идентификаторов:
                                     // IDOK, IDCANCEL, IDABORT, IDYES, IDNO, IDCLOSE
    // самый базовый конструктор, остальные - только обёртки к нему
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
    // методы
    procedure SetBufSize(Size: Cardinal);
    function Load: Boolean;
    function IndexOf(ID: Integer): Integer;
    procedure InternalSetItemTooltip(ID: Integer; const Tooltip: string);
    procedure InternalSetItemText(ID: Integer; const Text: string);
    function InternalGetItemText(ID: Integer): string;
    // Обработчики, вызываемые в моментах между созданием окна и показом его на экране и
    // между скрытием и уничтожением соответственно. Потомки должны перекрывать их для
    // выполнения своих действий над окном и контролами
    procedure DoBeforeShow; virtual;
    procedure DoBeforeClose(out Allow: Boolean; Code: Byte); virtual;
    procedure DoDialogProc(const Msg: TMsg; out Res: LRESULT); virtual;
    procedure DoBeforeMessage(const Msg: TMsg; out Res: LRESULT; out Handled: Boolean); virtual;
  private // для доступа из DefDialogProc
    // Оконная процедура
    function DialogProc(const Msg: TMsg): LRESULT;
  public
    // Событие при запуске диалога. Может использоваться для манипуляций с окном и
    // контролами в момент между созданием окна и показом его на экране (например, чтобы
    // перенести значения в нетекстовые контролы). Необходимо, поскольку до ShowModal
    // окно диалога попросту не существует
    OnBeforeShow: procedure(Sender: TResDialog) of object;
    // Событие при закрытии диалога. Может использоваться для манипуляций с окном и
    // контролами в момент между скрытием и уничтожением окна (например, чтобы
    // перенести значения из нетекстовых контролов). Необходимо,
    // поскольку после ShowModal окно диалога перестаёт существовать.
    // Установкой Allow в False можно запретить закрытие диалога.
    OnBeforeClose: procedure(Sender: TResDialog; out Allow: Boolean; Code: Byte) of object;
    // Событие реакции на оконное сообщение, должно установить Res в True, если сообщение обработано {}
    // Помимо системных сообщений, получает также внутренние сообщения:
    //   RDM_DLGOPENING (аналог WM_INITDIALOG) - между созданием и показом диалога
    //   RDM_DLGCLOSING (аналог WM_COMMAND и wParam из EndDlgIDs) - при закрытии
    //   RDM_DLGCLOSED  (аналог WM_NCDESTROY) - после закрытия
    OnDialogProc: procedure(Sender: TResDialog; const Msg: TMsg; out Res: LRESULT) of object;
    // Свой обработчик всех поступивших сообщений.
    // !! Вызывается ТОЛЬКО если диалог показан через методы Show или ShowModalCustom.
    // При этом запускается собственный цикл выборки сообщений (для метода Show его надо
    // запускать вручную через вызов ProcessMessages), который позволяет реагировать на
    // любое поступившее сообщение до обработки его функцией IsDialogMessage. Однако надо
    // помнить, что в этот обработчик будут направлены ВСЕ сообщения из очереди, включая
    // не относящиеся к диалоговому окну. Поэтому обработчик обязан проверять адресат (Msg.hwnd)
    // сообщения, чтобы не съесть чужое сообщение! (можно юзать if GetDlgCtrlID(Msg.hwnd) = ...)
    // Обработчик может установить Handled в True, чтобы прекратить обработку данного сообщения
    OnBeforeMessage: procedure(Sender: TResDialog; const Msg: TMsg; out Res: LRESULT; out Handled: Boolean) of object;

    // может служить для хранения указателей либо любых других данных
    Tag: NativeUInt;
    // стиль тултипов    ! Срабатывает только при показе диалога (если диалог уже открыт, ничего не меняет)
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

// *** Удобные обёртки WinAPI функций ***

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

// *** Состояние клавиш клавиатуры ***

// Проверка, нажата ли клавиша
function KeyPressed(KeyState: Byte): Boolean;
begin
  Result := KeyState and $80 <> 0;
end;

// Получить множество нажатых модификаторов из массива состояния клавиш.
// DifferRightLeft - различать левые и правые.
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

// Получить текущее множество нажатых модификаторов.
// DifferRightLeft - различать левые и правые.
function GetKeyboardShiftState(DifferRightLeft: Boolean = False): TShiftState;
var KeybState: TKeyboardState;
begin
  if not GetKeyboardState(KeybState)
    then Result := []
    else Result := KeyboardShiftState(KeybState, DifferRightLeft);
end;

// *** Сабклассинг контрола. Указатель на старую процедуру хранится в свойстве ***

const
  SubclassProp = 'Fr0sT_Subcl_WndProc';

type
  TSubclassData = record
    NewWndProc: TSubclassWndProc;
    OldWndProc: TFNWndProc;
  end;
  PSubclassData = ^TSubclassData;

// процедура-обёртка, которая будет вызываться для всех контролов
function SubclassWndProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  pSubclData: PSubclassData;
  Processed: Boolean;
begin
  Result := 0;
  pSubclData := PSubclassData(GetProp(hWnd, SubclassProp));
  if pSubclData = nil then Exit;

  // вызываем кастомный обработчик, если он не обработал - то стандартную оконную процедуру
  Processed := False;
  pSubclData.NewWndProc(hWnd, msg, wParam, lParam, Result, Processed);
  if not Processed then
    Result := CallWindowProc(pSubclData.OldWndProc, hWnd, msg, wParam, lParam);

  // если окно уничтожается - удалить свойство и освободить память, выделенную под запись
  if msg = WM_NCDESTROY then
  begin
    RemoveProp(hWnd, SubclassProp);
    if pSubclData <> nil then FreeMem(pSubclData);
  end;
end;

// Замена оконной процедуры у контрола на собственную
function SubclassControl(hWnd: HWND; NewWndProc: TSubclassWndProc): Boolean;
var
  pSubclData: PSubclassData;
  err: Cardinal;
begin
  Result := False;

  // проверяем, не сабклассили ли уже этот контрол
  pSubclData := PSubclassData(GetProp(hWnd, SubclassProp));
  if pSubclData <> nil then Exit;

  pSubclData := AllocMem(SizeOf(TSubclassData));
  // пробуем записать эти данные в свойство, если не получится - на выход
  if not SetProp(hWnd, SubclassProp, THandle(pSubclData)) then
  begin
    FreeMem(pSubclData);
    Exit;
  end;

  // меняем оконную процедуру у контрола, если не получится - на выход
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

// *** Диалоги ***

const
  DlgProp = 'Fr0sT_Dlg_HWnd';

// оконная процедура диалога
function DefDialogProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var dlg: TResDialog;
begin
  // инициализация - пихаем указатель на объект в свойства окна
  if msg = WM_INITDIALOG then
    SetProp(hWnd, DlgProp, lParam);
  // вытаскиваем указатель на объект
  dlg := TResDialog(Pointer(GetProp(hWnd, DlgProp)));
  // если окно закрывается, удалить prop (!) После этого dlg.DialogProc будет вызывана последний раз
  if msg = WM_NCDESTROY then
    RemoveProp(hWnd, DlgProp);
  // вызываем обработчик
  if dlg = nil
    then Result := LRESULT(False)
    else Result := dlg.DialogProc(MsgStruct(hWnd, msg, wParam, lParam));
end;

// вспомогательная функция, позволяющая создавать запись, требуемую для SetItemData,
// без дополнительных переменных
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

// Собственный цикл выборки сообщений, для немодальных диалогов либо для имитации модальности.
// Обрабатывает также и не относящиеся к своему окну сообщения, так что если необходимо
// блокировать их появление, надо "замораживать" вызывающее окно через EnableWindow
// (в методе ShowModalCustom это уже сделано)
procedure TResDialog.ProcessMessages;
var Msg: TMsg;
    Handled: Boolean;
    Res: LRESULT;
begin
  repeat
    // если окно закрылось - возвращаем управление вызывающей подпрограмме
    if not IsWindow(fDlgWnd) then Break;

    case Integer(GetMessage(Msg, 0, 0, 0)) of
       0: Break;
      -1: ;
      else
      begin
        // собственная обработка, если обработано - идем к следующему сообщению
        Handled := False;
        DoBeforeMessage(Msg, Res, Handled);
        if Handled then Continue;
        // стандартная обработка диалоговых сообщений
        if IsDialogMessage(fDlgWnd, Msg) then Continue;
        // все остальные сообщения - передаем системе
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  until False;
end;

// Загружает ресурс и получает указатель на него (только в режиме Persistent)
function TResDialog.Load: Boolean;
var hRes: HRSRC;
    hGl: HGLOBAL;
begin
  // уже загружено
  if fDlgTemplate <> nil then
    Exit(True);
  // загрузить ресурс
  hRes := FindResource(fInst, fDlgResID, RT_DIALOG);
  if hRes = 0 then Exit(False);
  hGl := LoadResource(fInst, hRes);
  if hGl = 0 then Exit(False);
  fDlgTemplate := LockResource(hGl);
  if fDlgTemplate = nil then Exit(False);
  Result := True;
end;

// Создание и показ окна диалога в модальном режиме.
// Возвращает 0 или -1 в случае ошибки
function TResDialog.ShowModal: INT_PTR;
begin
  // окно не должно существовать
  if fDlgWnd <> 0 then Exit(-1);

  fModalResult := -1;
  if not IsWindow(fParentWnd) then fParentWnd := 0;

  fMode := dmModal; // предварительно
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

// Создание и показ окна диалога в немодальном режиме
function TResDialog.Show(ShowCmd: Integer): Boolean;
begin
  fModalResult := -1;
  if not IsWindow(fParentWnd) then fParentWnd := 0;

  // если окно не существует, создаём его
  if fDlgWnd = 0 then
  begin
    if fPersistent then
      if Load then
        fDlgWnd := CreateDialogIndirectParam(fInst, fDlgTemplate^, fParentWnd, @DefDialogProc, LPARAM(Self))
      else
    else
      fDlgWnd := CreateDialogParam(fInst, fDlgResID, fParentWnd, @DefDialogProc, LPARAM(Self));
  end;

  // если окно успешно создано - показать его
  if fDlgWnd <> 0 then
  begin
    fMode := dmModeless;
    ShowWindow(fDlgWnd, ShowCmd); // ! Проверка на LastErr ненадежна - в программе с tab control значение
                                   // ставилось в ERROR_NOT_ENOUGH_MEMORY, хотя всё работало.
  end;

  Result := (fDlgWnd <> 0);
end;

// Создание и показ окна диалога в "собственной имитации" модального режима.
function TResDialog.ShowModalCustom: INT_PTR;
begin
  fModalResult := -1;
  if not IsWindow(fParentWnd) then fParentWnd := 0;

  // если окно не существует, создаём его
  if fDlgWnd = 0 then
  begin
    if fPersistent then
      if Load then
        fDlgWnd := CreateDialogIndirectParam(fInst, fDlgTemplate^, fParentWnd, @DefDialogProc, LPARAM(Self))
      else
    else
      fDlgWnd := CreateDialogParam(fInst, fDlgResID, fParentWnd, @DefDialogProc, LPARAM(Self));
  end;

  // если окно успешно создано - показать и запустить цикл выборки сообщений
  if fDlgWnd <> 0 then
  begin
    fMode := dmModalCustom;
    EnableWindow(fParentWnd, False);  // именно так достигается модальность
    ShowWindow(fDlgWnd, SW_SHOW);
    ProcessMessages;
    EnableWindow(fParentWnd, True);
  end;

  Result := fModalResult;
end;

// Обработчик оконной процедуры. Возвращает признак того, что сообщение обработано.
// Если возвращает False, сообщение направляется стандартному обработчику.
// Можно не заводить отдельных обработчиков DoDlgInit/Close - сообщения
// придут и в общий обработчик OnDialogProc
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
    // диалог создан, инициализация
    WM_INITDIALOG:
      begin
        fDlgWnd := Msg.hWnd;
        // ставим заголовок, только если он задан либо у окна ещё нет заголовка
        if (GetWindowTextLength(fDlgWnd) = 0) or (fCaption <> '') then
          SetWindowText(fDlgWnd, PChar(fCaption));
        // ставим надписи контролов
        for i := Low(fItemData) to High(fItemData) do
          InternalSetItemText(fItemData[i].ID, fItemData[i].Text);
        // создаем окно тултипов и добавляем элементы
        if fShowTooltips then
        begin
          Styles := 0; // необходимых стилей нет
          // перебираем дополнительные стили, и если они из списка стилей, добавляем в итоговое значение
          for style in TooltipStyles do
            if style in TooltipStyleSet then
              Styles := Styles or TooltipStyleValues[style];
          fTooltipWnd := CreateWindowEx(0, TOOLTIPS_CLASS, nil, Styles,
                                         0, 0, 0, 0, fDlgWnd, 0, HInstance, nil);
          SetWindowPos(fTooltipWnd, HWND_TOPMOST, 0, 0, 0, 0,
                       SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
          // добавляем все контролы с непустыми тултипами
          for i := Low(fItemData) to High(fItemData) do
            if fItemData[i].Tooltip <> '' then
              InternalSetItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
        end;
        DoBeforeShow;
        Result := LRESULT(True);
        DoDialogProc(MsgStruct(fDlgWnd, RDM_DLGOPENING, Msg.wParam, Msg.lParam), Result);
        Exit;
      end; // WM_INITDIALOG

    // Нотифай от дочернего контрола. Проверяем, если это нажата кнопка либо Escape, то завершить диалог.
    WM_COMMAND:
      begin
        EndDlgCmd := False;
        // Нажат Escape? HIWORD(wParam) = 0, LOWORD(wParam) = IDCANCEL
        if (HIWORD(Msg.wParam) = 0) and (LOWORD(Msg.wParam) = IDCANCEL) then
          EndDlgCmd := True
        // Нажата кнопка? HIWORD(wParam) = BN_CLICKED, LOWORD(wParam) = Btn_ID
        else if HIWORD(Msg.wParam) = BN_CLICKED then
        begin
          for i in fEndDlgBtnIDs do // ищем ID кнопки в списке ID, завершающих диалог
            if i = LOWORD(Msg.wParam) then
            begin
              EndDlgCmd := True;
              Break;
            end;
        end;
        // Диалог действительно завершается
        if EndDlgCmd then
        begin
          Result := LRESULT(True);
          // если не отмена, сохраняем тексты контролов
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

    // окно уничтожено, очистить данные
    WM_NCDESTROY:
      begin
        fMode := dmUndefined;
        DestroyWindow(fTooltipWnd); // уничтожить окно тултипов
        fTooltipWnd := 0;
        fDlgWnd := 0;
        Result := LRESULT(False);
        DoDialogProc(MsgStruct(fDlgWnd, RDM_DLGCLOSED, Msg.wParam, Msg.lParam), Result);
        Exit;
      end; // WM_NCDESTROY
  end; // case

  // все остальные случаи - предварительно ставим отрицательный результат и запускаем обработчик
  Result := LRESULT(False);
  DoDialogProc(Msg, Result);
end;

// set/get item text/tooltip

// найти в списке itemdata по ID
function TResDialog.IndexOf(ID: Integer): Integer;
begin
  for Result := Low(fItemData) to High(fItemData) do
    if fItemData[Result].ID = ID then
      Exit;
  Result := -1;
end;

// Добавляет тултип с текстом Tooltip для контрола с идентификатором ID.
// Если Tooltip - пустая строка, удаляет контрол из списка.
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
    Flags := TTF_IDISHWND or TTF_SUBCLASS; // необходимые флаги
    // перебираем дополнительные стили, и если они из списка флагов, добавляем в итоговое значение
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

// функция для получения текста из контрола диалога
//                        !!! варнингЪ !!!
// CharLen := GetWindowTextLength(hEdit) работает для всех, кроме элемента
// DateTimePicker, у которого обрезается последняя часть (год или секунды).
// GetWindowTextLength(hEdit) + 1 приводит к тому, что кушается и завершающий
// нулевой символ, поэтому нельзя использовать SetString(CharLen). Для универсальности
// сделано так: буфер выделяется заведомо больше на 1 символ, а для присваивания
// используется PChar, который берёт все символы до нулевого включительно.
function TResDialog.InternalGetItemText(ID: Integer): string;
var CharLen: Cardinal;
    pDest: PChar;
begin
  // определяем длину текста в контроле (в символах!!) (+завершающий нулевой!!)
  CharLen := GetWindowTextLength(Item[ID]) + 1;
  // расширяем буфер на строку и в любом случае заполняем весь буфер нулями
  SetBufSize(CharLen*SizeOf(Char));
  pDest := PChar(fBuf);
  GetDlgItemText(fDlgWnd, ID, pDest, CharLen);
  (pDest+CharLen-1)^ := #0; // чтобы уж наверняка!!
  Result := pDest;
end;

// процедура для установки текста контролу
procedure TResDialog.InternalSetItemText(ID: Integer; const Text: string);
begin
  SetDlgItemText(fDlgWnd, ID, PChar(Text));
end;

// присвоение заголовка окна; если диалог запущен, сразу и ставит его
procedure TResDialog.SetCaption(val: string);
begin
  if fCaption = val then Exit;
  fCaption := val;
  if fDlgWnd <> 0 then
    SetWindowText(fDlgWnd, PChar(fCaption));
end;

// Получение текста контрола, заданного через идентификатор.
// Если диалог открыт, сперва получает текст из контрола.
// Если в массиве данных с таким ID нет, добавляет новый элемент.
function TResDialog.GetItemText(ID: Integer; What: Integer): string;
var idx: Integer;
begin
  idx := IndexOf(ID);
  // элемента с указанным ID в массиве нет - добавляем
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

// Установка текста контрола, заданного через идентификатор.
// Если диалог открыт, сразу меняет текст на контроле.
// Если в массиве данных с таким ID нет, добавляет новый элемент.
procedure TResDialog.SetItemText(ID: Integer; What: Integer; const Text: string);
var idx: Integer;
begin
  idx := IndexOf(ID);
  // элемента с указанным ID в массиве нет - добавляем
  if idx = -1 then
  begin
    idx := Length(fItemData);
    SetLength(fItemData, idx + 1);
    fItemData[idx].ID := ID;
  end;
  // присваиваем значение
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

// Установка текстов сразу нескольких контролов, заданных через идентификатор.
// Принимает массив структур TDlgItemData. Если диалог открыт, сразу меняет текст на контроле
// ! заменяет имеющийся массив ! Чтобы не морочиться с очисткой, работает только
// если окно закрыто
// ! в этот же массив будут перенесены все значения с контролов при закрытии диалога
// ! именно array of TDlgItemData - иначе не даст создавать анонимный массив
procedure TResDialog.SetItemData(const ItemData: array of TDlgItemData);
var i: Integer;
begin
  // только когда окно закрыто
  if fDlgWnd <> 0 then Exit;
  SetLength(fItemData, 0); // очищаем имеющиеся значения
  SetLength(fItemData, Length(ItemData));
  for i := Low(ItemData) to High(ItemData) do
  begin
    fItemData[i] := ItemData[i];
    InternalSetItemText(fItemData[i].ID, fItemData[i].Text);
    // Если окно тултипов существует - добавить элемент. Выполняется даже для
    // пустых тултипов, чтобы иметь возможность очистить тултип.
    if fTooltipWnd <> 0 then
      InternalSetItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
  end;
end;

// Присваивает набор ID, завершающих диалог. Всегда первыми добавляет стандартный
// набор из StandardEndIDs. Можно передать пустой массив, тогда в списке будут
// только стандартные (это выполняется в конструкторе).
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

// показ/скрытие элемента
procedure TResDialog.SetItemVisible(ID: Integer; Visible: Boolean);
begin
  if Visible
    then ShowWindow(Item[ID], SW_SHOW)
    else ShowWindow(Item[ID], SW_HIDE);
end;

// определение видимости элемента
function TResDialog.GetItemVisible(ID: Integer): Boolean;
begin
  Result := IsWindowVisible(Item[ID]);
end;

// Смена родительского окна
procedure TResDialog.SetParent(Parent: HWND);
begin
  if fParentWnd = Parent then Exit;
  fParentWnd := Parent;
  // окно диалога открыто - меняем родителя у него
  if fDlgWnd <> 0 then
    Windows.SetParent(fDlgWnd, fParentWnd);
end;

// установка флага постоянности
procedure TResDialog.SetPersistent(val: Boolean);
begin
  if not val then
    fDlgTemplate := nil;
  fPersistent := val;
end;

// установка нового размера для буфера + заполнение его нулями в любом случае
procedure TResDialog.SetBufSize(Size: Cardinal);
begin
  // чтобы не писать проверку размеров и очистку, сделаем их тут
  // Size = 0 - особый случай, команда освободить буфер
  if (fBufSize >= Size) and (Size <> 0) then
  begin
    FillChar(fBuf^, fBufSize, 0);
    Exit;
  end;
  // освобождаем прежде выделенную память
  if (fBufSize <> 0) and (fBuf <> nil) then
  begin
    FreeMem(fBuf); fBuf := nil; fBufSize := 0;
  end;
  // распределяем заново и заполняем нулями, на всякий случай
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
