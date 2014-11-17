{
  Localization unit.
  Contains strings on Russian and English and a function returning a string for current language.
}

unit Lang;

interface

uses Winapi.Windows;

// String IDs
type
  TStringID =
  (
    // dialog window labels and captions
    idTitleFileProps,
    idTitleSelProps,
    idPgFileTitle,
    idPgDocTitle,
    idPgFileTextFileNameTip,
    idPgFileLabelRenameHint,
    idPgFileLabelPath,
    idPgFileLabelSize,
    idPgFileLabelCreated,
    idPgFileLabelModified,
    idPgFileTextSizePatt,
    idPgFileLabelWarnNotSaved,
    idPgFileLabelErrNotAFile,
    idPgFileBtnCopyPath,
    idPgFileBtnCopyPathTip,
    idPgFileBtnBrowse,
    idPgFileBtnBrowseTip,
    idPgDocLabelCodePage,
    idPgDocLabelChars,
    idPgDocLabelCharsTotal,
    idPgDocLabelCharsNoSp,
    idPgDocLabelLines,
    idPgDocLabelWords,
    idPgDocLabelLetters,
    idPgDocLabelLatin,
    idPgDocLabelSurr,
    idPgDocBtnCount,
    idPgDocBtnAbort,
    idMainBtnClose,
    idMainBtnReport,
    idMainBtnReportTip,
    //...
    // messages
    idMsgGetPropsFail,
    idMsgShowDlgFail,
    idMsgFileExists,
    idMsgCountNotCompleted,
    idMsgInvalidParam,
    // input box
    idInputBoxCaption,
    idInputBoxLabel,
    idInputBoxBtnOK,
    idInputBoxBtnCancel,
    // report
    idReportBasedOnSelection,
    // header
    idUnsaved,
    // other
    idBla
  );

procedure SetCurrLang(Lang: LANGID);
function LangString(StrId: TStringID): string;

implementation

type
  TLangStrings = array[TStringID] of string;

  TLangData = record
    LangId: LANGID;
    Strings: TLangStrings;
  end;

const
  LangData: array[1..2] of TLangData =
  (
    // en
    (
      LangId: LANG_ENGLISH;
      Strings: (
        // dialog window labels and captions
        'File statistics',
        'Selection statistics',
        'File',
        'Text',
        'Edit the text to rename current file',
        'Ctrl-S — rename the file',
        'Path',
        'Size',
        'Created',
        'Modified',
        '%s byte(s)',
        'There are some changes unsaved',
        'Document is not saved to a file',
        'Copy path',
        'Copy full file path to clipboard',
        'Browse',
        'Browse for file in Explorer',
        'Codepage',
        'Chars',
        '    total',
        '    without spaces',
        'Lines',
        'Words',
        'Letters',
        'Latin letters',
        'Surrogate pairs',
        'Count',
        'Abort',
        'Close',
        'Report',
        'Copy full statistics to clipboard',
        //...
        // messages
        'Error retrieving statistics',
        'Error showing the dialog',
        'File %s already exists',
        'Statistics estimation process wasn''t completed',
        'Invalid parameter: %s',
        // input box
        'Rename file',
        'Enter new file name',
        'OK',
        'Cancel',
        // report
        'Statistics estimated on selected part of text',
        // header
        'Unsaved',
        // other
        'Blabla'
      );
    ),
    // ru
    (
      LangId: LANG_RUSSIAN;
      Strings: (
        // dialog window labels and captions
        'Свойства файла',
        'Свойства выделенного текста',
        'Файл',
        'Текст',
        'Отредактируйте текст, чтобы переименовать файл',
        'Ctrl-S — переименовать файл',
        'Путь',
        'Размер',
        'Создан',
        'Изменён',
        '%s байт',
        'Есть несохранённые изменения',
        'Документ не сохранён в файл',
        'Копир. путь',
        'Скопировать полный путь в буфер обмена',
        'Обзор',
        'Открыть файл в проводнике',
        'Кодировка',
        'Символы',
        '    всего',
        '    без пробелов',
        'Строки',
        'Слова',
        'Буквы',
        'Латинские буквы',
        'Суррогатные пары',
        'Подсчитать',
        'Прервать',
        'Закрыть',
        'Отчёт',
        'Скопировать полную статистику в буфер обмена',
        //...
        // messages
        'Ошибка при получении свойств',
        'Ошибка при показе диалога',
        'Указанный файл %s уже существует',
        'Процесс подсчёта статистики не был завершён',
        'Неправильный параметр: %s',
        // input box
        'Переименовать файл',
        'Введите новое имя файла',
        'ОК',
        'Отмена',
        // report
        'Статистика рассчитана по выделенному фрагменту текста',
        // header
        'Несохранённый',
        // other
        'Блабла'
      );
    )
  );

var
  CurrLangId: LANGID = LANG_NEUTRAL;

procedure SetCurrLang(Lang: LANGID);
begin
  CurrLangId := Lang;
end;

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


end.
