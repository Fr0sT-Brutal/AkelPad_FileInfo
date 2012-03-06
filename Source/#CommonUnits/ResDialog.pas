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
// * add idyes, idno, ... (?) - or just post close message on custom close codes?
// * (?) исчезает хинт
// * добавить возможность указывать заголовок и иконку баллуна
// * коллбэки на хинт - ?

interface

uses Windows, Messages, CommCtrl;

// псевдо-сообщения, которые отправляются в обработчик OnDialogProc после
// создания, перед уничтожением и после уничтожения окна соответственно
const
  RDM_DLGOPENING = WM_APP + $F1;
  RDM_DLGCLOSING = WM_APP + $F2;
  RDM_DLGCLOSED  = WM_APP + $F3;

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
    fMode: TDlgMode;              // текущий режим показа окна
    fPersistent: Boolean;         // если True, диалог загружается только единожды через LockResource
    fDlgResName: string;          // название ресурса - сохраняем, чтобы строка не потерялась
    fDlgResID: LPTSTR;            // идентификатор ресурса
    fDlgTemplate: PDlgTemplate;   // указатель на загруженный ресурс (если fPersistent = True)
    fBuf: Pointer;                // буфер для получения текста контролов
    fBufSize: Cardinal;           // размер буфера
    fHInst: HINST;                // хэндл модуля, из которого загружается ресурс
    fParentHwnd, fDlgHwnd: HWND;  // хэндлы родительского и диалогового окон
    fTooltipHwnd: HWND;           // хэндл окна тултипов
    fShowTooltips: Boolean;       // показывать ли тултипы для контролов ! Срабатывает только при показе диалога (если диалог уже открыт, ничего не меняет)
    fCaption: string;             // заголовок окна
    fItemData: array of TDlgItemData;
    // самый базовый конструктор, остальные - только обёртки к нему
    constructor Create(const Instance: HINST; DlgResID: LPTSTR; ParentWnd: HWND); overload;
    // setters
    procedure SetCaption(val: string);
    procedure SetPersistent(val: Boolean);
    procedure SetParent(ParentHwnd: HWND);
    procedure SetDlgItemText(ID: Integer; What: Integer; const Text: string);
    // getters
    function GetDlgItem(ID: Integer): HWND;
    function GetDlgItemText(ID: Integer; What: Integer): string;
    // методы
    procedure SetBufSize(Size: Cardinal);
    function Load: Boolean;
    function IndexOf(ID: Integer): Integer;
    procedure InternalAddItemTooltip(ID: Integer; const Tooltip: string);
    procedure InternalSetDlgItemText(ID: Integer; const Text: string);
    function InternalGetDlgItemText(ID: Integer): string;
    // Обработчики, вызываемые в моментах между созданием окна и показом его на экране и
    // между скрытием и уничтожением соответственно. Потомки должны перекрывать их для
    // выполнения своих действий над окном и контролами
    procedure DoBeforeShow; virtual;
    procedure DoBeforeClose(out Allow: Boolean; Code: Byte); virtual;
    procedure DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT); virtual;
  private // для доступа из DefDialogProc
    // Оконная процедура
    function DialogProc(wnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
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
    //   RDM_DLGOPENING (аналог WM_INITDIALOG) - между созданием и показом диалога
    //   RDM_DLGCLOSING (аналог WM_COMMAND и wParam из EndDlgIDs) - при закрытии
    //   RDM_DLGCLOSED  (аналог WM_NCDESTROY) - после закрытия
    OnDialogProc: procedure(Sender: TResDialog; msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT) of object;

    // набор идентификаторов объектов, нажатие на которые завершает диалог
    // идентификаторы не должны быть больше 255!
    EndDlgIDs: set of Byte;
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
    procedure ProcessMessages;

    property DlgHwnd: HWND                 read fDlgHwnd;
    property ParentHwnd: HWND              read fParentHwnd    write SetParent;
    property TooltipHwnd: HWND             read fTooltipHwnd;
    property Caption: string               read fCaption       write SetCaption;
    property Persistent: Boolean           read fPersistent    write SetPersistent;
    property ShowTooltips: Boolean         read fShowTooltips  write fShowTooltips;

    property Item[ID: Integer]: HWND                          read GetDlgItem;
    property ItemText[ID: Integer]: string    index CODE_TEXT read GetDlgItemText write SetDlgItemText;
    property ItemTooltip[ID: Integer]: string index CODE_TTIP read GetDlgItemText write SetDlgItemText;
  end;

function ItemData(ID: Integer; const Text: string; const Tooltip: string = ''): TDlgItemData; inline;

implementation

const
  PropName = 'Fr0sT_Dlg_HWnd';

// оконная процедура диалога
function DefDialogProc(wnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var dlg: TResDialog;
begin
  // инициализация - пихаем указатель на объект в свойства окна
  if msg = WM_INITDIALOG then
    SetProp(wnd, PropName, lParam);
  // вытаскиваем указатель на объект
  dlg := TResDialog(Pointer(GetProp(wnd, PropName)));
  // если окно закрывается, удалить prop (!) После этого dlg.DialogProc будет вызывана последний раз
  if msg = WM_NCDESTROY then
    RemoveProp(wnd, PropName);
  // вызываем обработчик
  if dlg = nil
    then Result := LRESULT(False)
    else Result := dlg.DialogProc(wnd, msg, wParam, lParam);
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

// Загружает ресурс и получает указатель на него (только в режиме Persistent)
function TResDialog.Load: Boolean;
var hRes: HRSRC;
    hGl: HGLOBAL;
begin
  // уже загружено
  if fDlgTemplate <> nil then
    Exit(True);
  // загрузить ресурс
  hRes := FindResource(fHInst, fDlgResID, RT_DIALOG);
  if hRes = 0 then Exit(False);
  hGl := LoadResource(fHInst, hRes);
  if hGl = 0 then Exit(False);
  fDlgTemplate := LockResource(hGl);
  if fDlgTemplate = nil then Exit(False);
  Result := True;
end;

// Создание и показ окна диалога в модальном режиме.
// Возвращает 0 или -1 в случае ошибки
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

// Создание и показ окна диалога в немодальном режиме
function TResDialog.Show(ShowCmd: Integer): Boolean;
begin
  // если окно не существует, создаём его
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
  // показываем окно
  ShowWindow(fDlgHwnd, ShowCmd); // ! Проверка на LastErr ненадежна - в программе с tab control значение
                                 // ставилось в ERROR_NOT_ENOUGH_MEMORY, хотя всё работало.
  Result := True;
end;

// Обработчик оконной процедуры. Возвращает признак того, что сообщение обработано.
// Если возвращает False, сообщение направляется стандартному обработчику.
// Можно не заводить отдельных обработчиков DoDlgInit/Close - сообщения
// придут и в общий обработчик OnDialogProc
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
    // диалог создан, инициализация
    WM_INITDIALOG:
      begin
        fDlgHwnd := wnd;
        // ставим заголовок, только если он задан либо у окна ещё нет заголовка
        if (GetWindowTextLength(fDlgHwnd) = 0) or (fCaption <> '') then
          SetWindowText(fDlgHwnd, PChar(fCaption));
        // ставим надписи контролов
        for i := Low(fItemData) to High(fItemData) do
          InternalSetDlgItemText(fItemData[i].ID, fItemData[i].Text);
        // создаем окно тултипов и добавляем элементы
        if fShowTooltips then
        begin
          Styles := 0; // необходимых стилей нет
          // перебираем дополнительные стили, и если они из списка стилей, добавляем в итоговое значение
          for style in TooltipStyles do
            if style in TooltipStyleSet then
              Styles := Styles or TooltipStyleValues[style];
          fTooltipHwnd := CreateWindowEx(0, TOOLTIPS_CLASS, nil, Styles,
                                         0, 0, 0, 0, fDlgHwnd, 0, HInstance, nil);
          SetWindowPos(fTooltipHwnd, HWND_TOPMOST, 0, 0, 0, 0,
                       SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
          // добавляем все контролы с непустыми тултипами
          for i := Low(fItemData) to High(fItemData) do
            if fItemData[i].Tooltip <> '' then
              InternalAddItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
        end;
        DoBeforeShow;
        Result := LRESULT(True);
        DoDialogProc(RDM_DLGOPENING, wParam, lParam, Result);
        Exit;
      end;
    // нажат какой-то контрол в диалоге, проверяем, не кнопка завершения ли это.
    // если да, завершаем диалог
    WM_COMMAND:
      if LOWORD(wParam) in EndDlgIDs then
      begin
        Result := LRESULT(True);
        // если не отмена, получаем значения контролов
        if LOWORD(wParam) <> IDCANCEL then
          for i := Low(fItemData) to High(fItemData) do
            fItemData[i].Text := InternalGetDlgItemText(fItemData[i].ID);
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
    // окно уничтожено, очистить данные
    WM_NCDESTROY:
      begin
        fMode := dmUndefined;
        DestroyWindow(fTooltipHwnd); // уничтожить окно тултипов
        fTooltipHwnd := 0;
        fDlgHwnd := 0;
        fParentHwnd := 0; // на всякий случай обнуляем и хэндл родительского окна
        Result := LRESULT(False);
        DoDialogProc(RDM_DLGCLOSED, wParam, lParam, Result);
        Exit;
      end;
  end; // case
  // все остальные случаи - предварительно ставим отрицательный результат и запускаем обработчик
  Result := LRESULT(False);
  DoDialogProc(msg, wParam, lParam, Result);
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

// процедура для добавления тултипа с указанным текстом на контрол с указанным ID
procedure TResDialog.InternalAddItemTooltip(ID: Integer; const Tooltip: string);
var ti: TToolInfo;
    Flags: UINT;
    flag: TTooltipStyle;
begin
  FillChar(ti, SizeOf(ti), 0);
  ti.cbSize := SizeOf(ti);
  Flags := TTF_IDISHWND or TTF_SUBCLASS; // необходимые флаги
  // перебираем дополнительные стили, и если они из списка флагов, добавляем в итоговое значение
  for flag in TooltipStyles do
    if flag in TooltipFlagSet then
      Flags := Flags or TooltipStyleValues[flag];
  ti.uFlags := Flags;
  ti.hwnd := fDlgHwnd;
  ti.uId := Item[ID];
  ti.lpszText := PChar(Tooltip);
  SendMessage(fTooltipHwnd, TTM_ADDTOOL, 0, Windows.LPARAM(@ti));
end;

// функция для получения текста из контрола диалога
//                        !!! варнингЪ !!!
// CharLen := GetWindowTextLength(hEdit) работает для всех, кроме элемента
// DateTimePicker, у которого обрезается последняя часть (год или секунды).
// GetWindowTextLength(hEdit) + 1 приводит к тому, что кушается и завершающий
// нулевой символ, поэтому нельзя использовать SetString(CharLen). Для универсальности
// сделано так: буфер выделяется заведомо больше на 1 символ, а для присваивания
// используется PChar, который берёт все символы до нулевого включительно.
function TResDialog.InternalGetDlgItemText(ID: Integer): string;
var CharLen: Cardinal;
    pDest: PChar;
begin
  // определяем длину текста в контроле (в символах!!) (+завершающий нулевой!!)
  CharLen := GetWindowTextLength(Item[ID]) + 1;
  // расширяем буфер на строку и в любом случае заполняем весь буфер нулями
  SetBufSize(CharLen*SizeOf(Char));
  pDest := PChar(fBuf);
  Windows.GetDlgItemText(fDlgHwnd, ID, pDest, CharLen);
  (pDest+CharLen-1)^ := #0; // чтобы уж наверняка!!
  Result := pDest;
end;

// процедура для установки текста контролу
procedure TResDialog.InternalSetDlgItemText(ID: Integer; const Text: string);
begin
  Windows.SetDlgItemText(fDlgHwnd, ID, PChar(Text));
end;

// присвоение заголовка окна; если диалог запущен, сразу и ставит его
procedure TResDialog.SetCaption(val: string);
begin
  if fCaption = val then Exit;
  fCaption := val;
  if fDlgHwnd <> 0 then
    SetWindowText(fDlgHwnd, PChar(fCaption));
end;

// Получение текста контрола, заданного через идентификатор.
// Если диалог открыт, сперва получает текст из контрола.
function TResDialog.GetDlgItemText(ID: Integer; What: Integer): string;
var idx: Integer;
begin
  idx := IndexOf(ID);
  if idx <> -1 then
  begin
    if fDlgHwnd <> 0 then
      fItemData[idx].Text := InternalGetDlgItemText(fItemData[idx].ID);
    Result := fItemData[idx].Text;
  end
  else
    Result := '';
end;

// Установка текста контрола, заданного через идентификатор.
// Если диалог открыт, сразу меняет текст на контроле.
// Если в массиве данных с таким ID нет, добавляет новый элемент.
procedure TResDialog.SetDlgItemText(ID: Integer; What: Integer; const Text: string);
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
        if fDlgHwnd <> 0 then
          InternalSetDlgItemText(fItemData[idx].ID, fItemData[idx].Text);
      end;
    CODE_TTIP:
      begin
        fItemData[idx].Tooltip := Text;
        if fTooltipHwnd <> 0 then
          InternalAddItemTooltip(fItemData[idx].ID, fItemData[idx].Tooltip);
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
  if fDlgHwnd <> 0 then Exit;
  SetLength(fItemData, 0); // очищаем имеющиеся значения
  SetLength(fItemData, Length(ItemData));
  for i := Low(ItemData) to High(ItemData) do
  begin
    fItemData[i] := ItemData[i];
    InternalSetDlgItemText(fItemData[i].ID, fItemData[i].Text);
    if fTooltipHwnd <> 0 then
      InternalAddItemTooltip(fItemData[i].ID, fItemData[i].Tooltip);
  end;
end;

// Смена родительского окна
procedure TResDialog.SetParent(ParentHwnd: HWND);
begin
  if fParentHwnd = ParentHwnd then Exit;
  fParentHwnd := ParentHwnd;
  // окно диалога открыто - меняем родителя у него
  if fDlgHwnd <> 0 then
    Windows.SetParent(fDlgHwnd, fParentHwnd);
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
function TResDialog.GetDlgItem(ID: Integer): HWND;
begin
  Result := Windows.GetDlgItem(fDlgHwnd, ID);
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
