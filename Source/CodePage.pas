unit CodePage;

interface
uses
  iniFiles,sysutils;

type
  CodePageInfo =record
   CodePage: integer;
   Name:string;
  end;

const
      CodePages: array[0..151] of CodePageInfo = //Thank you John Herbster
        (
            (CodePage: 037;         Name: 'IBM037'),                //IBM EBCDIC US-Canada
            (CodePage: 437;         Name: 'IBM437'),                //OEM United States
            (CodePage: 500;         Name: 'IBM500'),                //IBM EBCDIC International
            (CodePage: 708;         Name: 'ASMO-708'),          //Arabic (ASMO 708)
            (CodePage: 709;         Name: ''),                      //Arabic (ASMO-449+, BCON V4)
            (CodePage: 710;         Name: ''),                      //Arabic - Transparent Arabic
            (CodePage: 720;         Name: 'DOS-720'),               //Arabic (Transparent ASMO); Arabic (DOS)
            (CodePage: 737;         Name: 'ibm737'),                //OEM Greek (formerly 437G); Greek (DOS)
            (CodePage: 775;         Name: 'ibm775'),                //OEM Baltic; Baltic (DOS)
            (CodePage: 850;         Name: 'ibm850'),                //OEM Multilingual Latin 1; Western European (DOS)
            (CodePage: 852;         Name: 'ibm852'),                //OEM Latin 2; Central European (DOS)
            (CodePage: 855;         Name: 'IBM855'),                //OEM Cyrillic (primarily Russian)
            (CodePage: 857;         Name: 'ibm857'),                //OEM Turkish; Turkish (DOS)
            (CodePage: 858;         Name: 'IBM00858'),          //OEM Multilingual Latin 1 + Euro symbol
            (CodePage: 860;         Name: 'IBM860'),                //OEM Portuguese; Portuguese (DOS)
            (CodePage: 861;         Name: 'ibm861'),                //OEM Icelandic; Icelandic (DOS)
            (CodePage: 862;         Name: 'DOS-862'),               //OEM Hebrew; Hebrew (DOS)
            (CodePage: 863;         Name: 'IBM863'),                //OEM French Canadian; French Canadian (DOS)
            (CodePage: 864;         Name: 'IBM864'),                //OEM Arabic; Arabic (864)
            (CodePage: 865;         Name: 'IBM865'),                //OEM Nordic; Nordic (DOS)
            (CodePage: 866;         Name: 'cp866'),             //OEM Russian; Cyrillic (DOS)
            (CodePage: 869;         Name: 'ibm869'),                //OEM Modern Greek; Greek, Modern (DOS)
            (CodePage: 870;         Name: 'IBM870'),                //IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2
            (CodePage: 874;         Name: 'windows-874'),       //ANSI/OEM Thai (ISO 8859-11); Thai (Windows)
            (CodePage: 875;         Name: 'cp875'),             //IBM EBCDIC Greek Modern
            (CodePage: 932;         Name: 'shift_jis'),         //ANSI/OEM Japanese; Japanese (Shift-JIS)
            (CodePage: 936;         Name: 'gb2312'),                //ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)
            (CodePage: 949;         Name: 'ks_c_5601-1987'),    //ANSI/OEM Korean (Unified Hangul Code)
            (CodePage: 950;         Name: 'big5'),                  //ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)
            (CodePage: 1026;        Name: 'IBM1026'),               //IBM EBCDIC Turkish (Latin 5)
            (CodePage: 1047;        Name: 'IBM01047'),          //IBM EBCDIC Latin 1/Open System
            (CodePage: 1140;        Name: 'IBM01140'),          //IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro)
            (CodePage: 1141;        Name: 'IBM01141'),          //IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro)
            (CodePage: 1142;        Name: 'IBM01142'),          //IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro)
            (CodePage: 1143;        Name: 'IBM01143'),          //IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro)
            (CodePage: 1144;        Name: 'IBM01144'),          //IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro)
            (CodePage: 1145;        Name: 'IBM01145'),          //IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro)
            (CodePage: 1146;        Name: 'IBM01146'),          //IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro)
            (CodePage: 1147;        Name: 'IBM01147'),          //IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro)
            (CodePage: 1148;        Name: 'IBM01148'),          //IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro)
            (CodePage: 1149;        Name: 'IBM01149'),          //IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro)
            (CodePage: 1200;        Name: 'utf-16'),                //Unicode UTF-16, little endian byte order (BMP of ISO 10646); available only to managed applications
            (CodePage: 1201;        Name: 'unicodeFFFE'),       //Unicode UTF-16, big endian byte order; available only to managed applications
            (CodePage: 1250;        Name: 'windows-1250'),      //ANSI Central European; Central European (Windows)
            (CodePage: 1251;        Name: 'windows-1251'),      //ANSI Cyrillic; Cyrillic (Windows)
            (CodePage: 1252;        Name: 'windows-1252'),      //ANSI Latin 1; Western European (Windows)
            (CodePage: 1253;        Name: 'windows-1253'),      //ANSI Greek; Greek (Windows)
            (CodePage: 1254;        Name: 'windows-1254'),      //ANSI Turkish; Turkish (Windows)
            (CodePage: 1255;        Name: 'windows-1255'),      //ANSI Hebrew; Hebrew (Windows)
            (CodePage: 1256;        Name: 'windows-1256'),      //ANSI Arabic; Arabic (Windows)
            (CodePage: 1257;        Name: 'windows-1257'),      //ANSI Baltic; Baltic (Windows)
            (CodePage: 1258;        Name: 'windows-1258'),      //ANSI/OEM Vietnamese; Vietnamese (Windows)
            (CodePage: 1361;        Name: 'Johab'),             //Korean (Johab)
            (CodePage: 10000;   Name: 'macintosh'),         //MAC Roman; Western European (Mac)
            (CodePage: 10001;   Name: 'x-mac-japanese'),        //Japanese (Mac)
            (CodePage: 10002;   Name: 'x-mac-chinesetrad'), //MAC Traditional Chinese (Big5); Chinese Traditional (Mac)
            (CodePage: 10003;   Name: 'x-mac-korean'),          //Korean (Mac)
            (CodePage: 10004;   Name: 'x-mac-arabic'),          //Arabic (Mac)
            (CodePage: 10005;   Name: 'x-mac-hebrew'),          //Hebrew (Mac)
            (CodePage: 10006;   Name: 'x-mac-greek'),           //Greek (Mac)
            (CodePage: 10007;   Name: 'x-mac-cyrillic'),        //Cyrillic (Mac)
            (CodePage: 10008;   Name: 'x-mac-chinesesimp'), //MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac)
            (CodePage: 10010;   Name: 'x-mac-romanian'),        //Romanian (Mac)
            (CodePage: 10017;   Name: 'x-mac-ukrainian'),       //Ukrainian (Mac)
            (CodePage: 10021;   Name: 'x-mac-thai'),                //Thai (Mac)
            (CodePage: 10029;   Name: 'x-mac-ce'),              //MAC Latin 2; Central European (Mac)
            (CodePage: 10079;   Name: 'x-mac-icelandic'),       //Icelandic (Mac)
            (CodePage: 10081;   Name: 'x-mac-turkish'),         //Turkish (Mac)
            (CodePage: 10082;   Name: 'x-mac-croatian'),        //Croatian (Mac)
            (CodePage: 12000;   Name: 'utf-32'),                    //Unicode UTF-32, little endian byte order; available only to managed applications
            (CodePage: 12001;   Name: 'utf-32BE'),              //Unicode UTF-32, big endian byte order; available only to managed applications
            (CodePage: 20000;   Name: 'x-Chinese_CNS'),         //CNS Taiwan; Chinese Traditional (CNS)
            (CodePage: 20001;   Name: 'x-cp20001'),             //TCA Taiwan
            (CodePage: 20002;   Name: 'x_Chinese-Eten'),        //Eten Taiwan; Chinese Traditional (Eten)
            (CodePage: 20003;   Name: 'x-cp20003'),             //IBM5550 Taiwan
            (CodePage: 20004;   Name: 'x-cp20004'),             //TeleText Taiwan
            (CodePage: 20005;   Name: 'x-cp20005'),             //Wang Taiwan
            (CodePage: 20105;   Name: 'x-IA5'),                 //IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5)
            (CodePage: 20106;   Name: 'x-IA5-German'),          //IA5 German (7-bit)
            (CodePage: 20107;   Name: 'x-IA5-Swedish'),         //IA5 Swedish (7-bit)
            (CodePage: 20108;   Name: 'x-IA5-Norwegian'),       //IA5 Norwegian (7-bit)
            (CodePage: 20127;   Name: 'us-ascii'),              //US-ASCII (7-bit)
            (CodePage: 20261;   Name: 'x-cp20261'),             //T.61
            (CodePage: 20269;   Name: 'x-cp20269'),             //ISO 6937 Non-Spacing Accent
            (CodePage: 20273;   Name: 'IBM273'),                    //IBM EBCDIC Germany
            (CodePage: 20277;   Name: 'IBM277'),                    //IBM EBCDIC Denmark-Norway
            (CodePage: 20278;   Name: 'IBM278'),                    //IBM EBCDIC Finland-Sweden
            (CodePage: 20280;   Name: 'IBM280'),                    //IBM EBCDIC Italy
            (CodePage: 20284;   Name: 'IBM284'),                    //IBM EBCDIC Latin America-Spain
            (CodePage: 20285;   Name: 'IBM285'),                    //IBM EBCDIC United Kingdom
            (CodePage: 20290;   Name: 'IBM290'),                    //IBM EBCDIC Japanese Katakana Extended
            (CodePage: 20297;   Name: 'IBM297'),                    //IBM EBCDIC France
            (CodePage: 20420;   Name: 'IBM420'),                    //IBM EBCDIC Arabic
            (CodePage: 20423;   Name: 'IBM423'),                    //IBM EBCDIC Greek
            (CodePage: 20424;   Name: 'IBM424'),                    //IBM EBCDIC Hebrew
            (CodePage: 20833;   Name: 'x-EBCDIC-KoreanExtended'),           //IBM EBCDIC Korean Extended
            (CodePage: 20838;   Name: 'IBM-Thai'),              //IBM EBCDIC Thai
            (CodePage: 20866;   Name: 'koi8-r'),                    //Russian (KOI8-R); Cyrillic (KOI8-R)
            (CodePage: 20871;   Name: 'IBM871'),                    //IBM EBCDIC Icelandic
            (CodePage: 20880;   Name: 'IBM880'),                    //IBM EBCDIC Cyrillic Russian
            (CodePage: 20905;   Name: 'IBM905'),                    //IBM EBCDIC Turkish
            (CodePage: 20924;   Name: 'IBM00924'),              //IBM EBCDIC Latin 1/Open System (1047 + Euro symbol)
            (CodePage: 20932;   Name: 'EUC-JP'),                    //Japanese (JIS 0208-1990 and 0212-1990)
            (CodePage: 20936;   Name: 'x-cp20936'),             //Simplified Chinese (GB2312); Chinese Simplified (GB2312-80)
            (CodePage: 20949;   Name: 'x-cp20949'),             //Korean Wansung
            (CodePage: 21025;   Name: 'cp1025'),                    //IBM EBCDIC Cyrillic Serbian-Bulgarian
            (CodePage: 21027;   Name: ''),                          //(deprecated)
            (CodePage: 21866;   Name: 'koi8-u'),                    //Ukrainian (KOI8-U); Cyrillic (KOI8-U)
            (CodePage: 28591;   Name: 'iso-8859-1'),                //ISO 8859-1 Latin 1; Western European (ISO)
            (CodePage: 28592;   Name: 'iso-8859-2'),                //ISO 8859-2 Central European; Central European (ISO)
            (CodePage: 28593;   Name: 'iso-8859-3'),                //ISO 8859-3 Latin 3
            (CodePage: 28594;   Name: 'iso-8859-4'),                //ISO 8859-4 Baltic
            (CodePage: 28595;   Name: 'iso-8859-5'),                //ISO 8859-5 Cyrillic
            (CodePage: 28596;   Name: 'iso-8859-6'),                //ISO 8859-6 Arabic
            (CodePage: 28597;   Name: 'iso-8859-7'),                //ISO 8859-7 Greek
            (CodePage: 28598;   Name: 'iso-8859-8'),                //ISO 8859-8 Hebrew; Hebrew (ISO-Visual)
            (CodePage: 28599;   Name: 'iso-8859-9'),                //ISO 8859-9 Turkish
            (CodePage: 28603;   Name: 'iso-8859-13'),           //ISO 8859-13 Estonian
            (CodePage: 28605;   Name: 'iso-8859-15'),           //ISO 8859-15 Latin 9
            (CodePage: 29001;   Name: 'x-Europa'),              //Europa 3
            (CodePage: 38598;   Name: 'iso-8859-8-i'),          //ISO 8859-8 Hebrew; Hebrew (ISO-Logical)
            (CodePage: 50220;   Name: 'iso-2022-jp'),           //ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)
            (CodePage: 50221;   Name: 'csISO2022JP'),           //ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)
            (CodePage: 50222;   Name: 'iso-2022-jp'),           //ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)
            (CodePage: 50225;   Name: 'iso-2022-kr'),           //ISO 2022 Korean
            (CodePage: 50227;   Name: 'x-cp50227'),             //ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)
            (CodePage: 50229;   Name: ''),                          //ISO 2022 Traditional Chinese
            (CodePage: 50930;   Name: ''),                          //EBCDIC Japanese (Katakana) Extended
            (CodePage: 50931;   Name: ''),                          //EBCDIC US-Canada and Japanese
            (CodePage: 50933;   Name: ''),                          //EBCDIC Korean Extended and Korean
            (CodePage: 50935;   Name: ''),                          //EBCDIC Simplified Chinese Extended and Simplified Chinese
            (CodePage: 50936;   Name: ''),                          //EBCDIC Simplified Chinese
            (CodePage: 50937;   Name: ''),                          //EBCDIC US-Canada and Traditional Chinese
            (CodePage: 50939;   Name: ''),                          //EBCDIC Japanese (Latin) Extended and Japanese
            (CodePage: 51932;   Name: 'euc-jp'),                    //EUC Japanese
            (CodePage: 51936;   Name: 'EUC-CN'),                    //EUC Simplified Chinese; Chinese Simplified (EUC)
            (CodePage: 51949;   Name: 'euc-kr'),                    //EUC Korean
            (CodePage: 51950;   Name: ''),                          //EUC Traditional Chinese
            (CodePage: 52936;   Name: 'hz-gb-2312'),                //HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)
            (CodePage: 54936;   Name: 'GB18030'),                   //Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)
            (CodePage: 57002;   Name: 'x-iscii-de'),                //ISCII Devanagari
            (CodePage: 57003;   Name: 'x-iscii-be'),                //ISCII Bangla
            (CodePage: 57004;   Name: 'x-iscii-ta'),                //ISCII Tamil
            (CodePage: 57005;   Name: 'x-iscii-te'),                //ISCII Telugu
            (CodePage: 57006;   Name: 'x-iscii-as'),                //ISCII Assamese
            (CodePage: 57007;   Name: 'x-iscii-or'),                //ISCII Odia
            (CodePage: 57008;   Name: 'x-iscii-ka'),                //ISCII Kannada
            (CodePage: 57009;   Name: 'x-iscii-ma'),                //ISCII Malayalam
            (CodePage: 57010;   Name: 'x-iscii-gu'),                //ISCII Gujarati
            (CodePage: 57011;   Name: 'x-iscii-pa'),                //ISCII Punjabi
            (CodePage: 65000;   Name: 'utf-7'),                 //Unicode (UTF-7)
            (CodePage: 65001;   Name: 'utf-8')                      //Unicode (UTF-8)
    );

function GetCodePage(id:integer):CodePageInfo;

implementation

function GetCodePage(id:integer):CodePageInfo; overload;
const
  NoneResult:CodePageInfo = (CodePage: 0; Name: 'none');
var
  i:integer;
begin
  Result := NoneResult;
  for i:=0 to Length(CodePages)-1 do begin
    if CodePages[i].CodePage = id then begin
      Result := CodePages[i];
      Exit;
    end;
  end;
end;

function GetCodePage(const name:string):CodePageInfo; overload;
const
  NoneResult:CodePageInfo = (CodePage: 0; Name: 'none');
var
  i:integer;
begin
  Result := NoneResult;
  for i:=0 to Length(CodePages)-1 do begin
    if CodePages[i].Name = name then begin
      Result := CodePages[i];
      Exit;
    end;
  end;
end;

end.
