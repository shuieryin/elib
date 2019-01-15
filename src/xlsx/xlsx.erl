-module(xlsx).

-export([create/2]).

-record(xlsx, {
    tmp :: file:filename_all(),
    files :: [file:filename_all()],
    sheets :: [[string() | binary()]]
}).
%% xlsx:create([{"x1", [["2","3","3","5"], ["hhh", "32"]], []}], "priv/admin_html/ss.xlsx").


%%--------------------------------------------------------------------
%% @doc
%% generic method to create xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec create(
    Sheets :: [[iodata()]],
    OutFile :: file:filename_all()
) -> ok | {error, Reason :: term()}.
create(Sheets, OutFile) ->
    Xlsx0 = xlsx_util:new(OutFile),
    Xlsx1 = Xlsx0#xlsx{sheets = Sheets},
    {ok, Xlsx2, SharedStrings} = lists:foldl(
        fun({Idx, {_Name, Rows}}, {ok, Acc, AccSharedStrings}) ->
            I = integer_to_list(Idx),
            {SheetData, UpdatedAccSharedStrings} = xlsx_sheet:encode_sheet(Rows, AccSharedStrings),

            {ok, AccXlsx} = xlsx_util:write(Acc, "xl/worksheets/sheet" ++ I ++ ".xml", SheetData),
            {ok, AccXlsx, UpdatedAccSharedStrings}
        end,
        {ok, Xlsx1, #{}},
        numbered_sheets(Xlsx1)),
    {ok, Xlsx3} = add_shared_strings(Xlsx2, SharedStrings),

    {ok, XlsxNew} = lists:foldl(
        fun(F, {ok, X}) -> F(X) end,
        {ok, Xlsx3},
        [
            fun add_doc_props/1,
            fun add_relationship_part/1,
            fun add_styles/1,
            fun add_workbook_relationship_part/1,
            fun add_content_types/1,
            fun add_workbook_part/1
        ]),
    xlsx_util:write(XlsxNew, OutFile).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% sequence each sheet
%%
%% @end
%%--------------------------------------------------------------------
-spec numbered_sheets(Xlsx :: #xlsx{}) -> Sheet :: term().
numbered_sheets(#xlsx{sheets = Sheets}) ->
    lists:zip(lists:seq(1, length(Sheets)), Sheets).


%%--------------------------------------------------------------------
%% @doc
%% workbook.xml for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec add_workbook_part(Xlsx :: #xlsx{}) -> {ok, X :: #xlsx{}}.
add_workbook_part(Xlsx) ->
    xlsx_util:write(
        Xlsx, "xl/workbook.xml",
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
            "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" ",
            "xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" mc:Ignorable=\"x15 xr xr6 xr10 xr2\" ",
            "xmlns:x15=\"http://schemas.microsoft.com/office/spreadsheetml/2010/11/main\" ",
            "xmlns:xr=\"http://schemas.microsoft.com/office/spreadsheetml/2014/revision\" ",
            "xmlns:xr6=\"http://schemas.microsoft.com/office/spreadsheetml/2016/revision6\" ",
            "xmlns:xr10=\"http://schemas.microsoft.com/office/spreadsheetml/2016/revision10\" ",
            "xmlns:xr2=\"http://schemas.microsoft.com/office/spreadsheetml/2015/revision2\">",
            "<fileVersion appName=\"xl\" lastEdited=\"7\" lowestEdited=\"7\" rupBuild=\"11208\"/>",
            "<workbookPr defaultThemeVersion=\"166925\"/> ",
            "<xr:revisionPtr revIDLastSave=\"0\" documentId=\"8_{7FF973DD-9A0E-914B-BC53-1412BB3F9746}\" ",
            "xr6:coauthVersionLast=\"40\" xr6:coauthVersionMax=\"40\" ",
            "xr10:uidLastSave=\"{00000000-0000-0000-0000-000000000000}\"/> ",
            "<bookViews>",
            "<workbookView xWindow=\"0\" yWindow=\"460\" windowWidth=\"28800\" windowHeight=\"17540\" ",
            "xr2:uid=\"{00000000-000D-0000-FFFF-FFFF00000000}\"/> ",
            "</bookViews>",
            "<sheets>",
            lists:map(fun({Idx, {SheetName, _Rows}}) ->
                I = integer_to_list(Idx),
                ["<sheet name=\"", z_html:escape(SheetName), "\" sheetId=\"", I, "\" r:id=\"sheet", I, "\"/>"]
                      end,
                numbered_sheets(Xlsx)),
            "</sheets>",
            "<calcPr calcId=\"0\"/>",
            "<fileRecoveryPr repairLoad=\"1\"/>",
            "</workbook>"]).


%%--------------------------------------------------------------------
%% @doc
%% workbook.xml.rels for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec add_workbook_relationship_part(Xlsx :: #xlsx{}) -> {ok, X :: #xlsx{}}.
add_workbook_relationship_part(Xlsx) ->
    xlsx_util:write(
        Xlsx, "xl/_rels/workbook.xml.rels",
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
            "<Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>",
            "<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme\" Target=\"theme/theme1.xml\"/>",
            lists:map(fun({Idx, _Sheet}) ->
                I = integer_to_list(Idx),
                ["<Relationship Id=\"sheet", I, "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet", I, ".xml\"/>"]
                      end,
                numbered_sheets(Xlsx)),
            "<Relationship Id=\"rId4\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings\" Target=\"sharedStrings.xml\"/>",
            "</Relationships>"
        ]).


%%--------------------------------------------------------------------
%% @doc
%% content_type.xml for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec add_content_types(Xlsx :: #xlsx{}) -> {ok, X :: #xlsx{}}.
add_content_types(Xlsx) ->
    xlsx_util:write(
        Xlsx, "[Content_Types].xml",
        ["<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
            "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"> ",
            "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/> ",
            "<Default Extension=\"xml\" ContentType=\"application/xml\"/>",
            "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/> ",
            lists:map(fun({Idx, _Sheet}) ->
                I = integer_to_list(Idx),
                ["<Override PartName=\"/xl/worksheets/sheet", I, ".xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"]
                      end,
                numbered_sheets(Xlsx)),
            "<Override PartName=\"/xl/theme/theme1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.theme+xml\"/> ",
            "<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/> ",
            "<Override PartName=\"/xl/sharedStrings.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml\"/> ",
            "<Override PartName=\"/docProps/core.xml\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\"/> ",
            "<Override PartName=\"/docProps/app.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.extended-properties+xml\"/> ",
            "</Types>"
        ]).


%%--------------------------------------------------------------------
%% @doc
%% .rels file for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec add_relationship_part(Xlsx :: #xlsx{}) -> {ok, X :: #xlsx{}}.
add_relationship_part(Xlsx) ->
    xlsx_util:write(
        Xlsx, "_rels/.rels",
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
            "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
            "  <Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties\" Target=\"docProps/app.xml\"/>",
            "  <Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties\" Target=\"docProps/core.xml\"/>",
            "  <Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>",
            "</Relationships>"
        ]).

%%--------------------------------------------------------------------
%% @doc
%% doc_props.xml for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec add_doc_props(Xlsx :: #xlsx{}) -> {ok, X :: #xlsx{}}.
add_doc_props(Xlsx) ->
    {ok, X2} = xlsx_util:write(
        Xlsx, "docProps/core.xml",
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" ",
            "xmlns:dc=\"http://purl.org/dc/elements/1.1/\" ",
            "xmlns:dcterms=\"http://purl.org/dc/terms/\" ",
            "xmlns:dcmitype=\"http://purl.org/dc/dcmitype/\" ",
            "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> ",
            "<cp:revision>0</cp:revision>",
            "<dcterms:created xsi:type=\"dcterms:W3CDTF\">2010-07-20T14:30:58Z</dcterms:created>",
            "<dcterms:modified xsi:type=\"dcterms:W3CDTF\">2019-01-14T05:31:08Z</dcterms:modified>",
            "</cp:coreProperties>"
        ]),
    xlsx_util:write(
        X2, "docProps/app.xml",
        ["<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<Properties xmlns=\"http://schemas.openxmlformats.org/officeDocument/2006/extended-properties\" ",
            "xmlns:vt=\"http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes\">",
            "<TotalTime>0</TotalTime>",
            "<Application>Microsoft Macintosh Excel</Application>",
            "<DocSecurity>0</DocSecurity>",
            "<ScaleCrop>false</ScaleCrop>",
            "<HeadingPairs>",
            "<vt:vector size=\"2\" baseType=\"variant\">",
            "<vt:variant>",
            "<vt:lpstr>Worksheets</vt:lpstr>",
            "</vt:variant>",
            "<vt:variant>",
            "<vt:i4>1</vt:i4>",
            "</vt:variant>",
            "</vt:vector>",
            "</HeadingPairs>",
            "<TitlesOfParts>",
            "<vt:vector size=\"1\" baseType=\"lpstr\">",
            "<vt:lpstr>Sheet 1</vt:lpstr>",
            "</vt:vector>",
            "</TitlesOfParts>",
            "<LinksUpToDate>false</LinksUpToDate>",
            "<SharedDoc>false</SharedDoc>",
            "<HyperlinksChanged>false</HyperlinksChanged>",
            "<AppVersion>16.0300</AppVersion>",
            "</Properties>"
        ]).


%%--------------------------------------------------------------------
%% @doc
%% style.xml for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec add_styles(Xlsx :: #xlsx{}) -> {ok, X :: #xlsx{}}.
add_styles(Xlsx) ->
    xlsx_util:write(
        Xlsx, "xl/styles.xml",
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
            "xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" mc:Ignorable=\"x14ac x16r2 xr\" ",
            "xmlns:x14ac=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac\" ",
            "xmlns:x16r2=\"http://schemas.microsoft.com/office/spreadsheetml/2015/02/main\" ",
            "xmlns:xr=\"http://schemas.microsoft.com/office/spreadsheetml/2014/revision\">",
            "<fonts count=\"2\" x14ac:knownFonts=\"1\">",
            "<font>",
            "<sz val=\"10\"/>",
            "<name val=\"Arial\"/>",
            "<family val=\"2\"/>",
            "</font>",
            "<font>",
            "<sz val=\"10\"/>",
            "<name val=\"Arial\"/>",
            "<family val=\"2\"/>",
            "</font>",
            "</fonts>",
            "<fills count=\"2\">",
            "<fill>",
            "<patternFill patternType=\"none\"/>",
            "</fill>",
            "<fill>",
            "<patternFill patternType=\"gray125\"/>",
            "</fill>",
            "</fills>",
            "<borders count=\"1\">",
            "<border>",
            "<left/>",
            "<right/>",
            "<top/>",
            "<bottom/>",
            "<diagonal/>",
            "</border>",
            "</borders>",
            "<cellStyleXfs count=\"1\">",
            "<xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/>",
            "</cellStyleXfs>",
            "<cellXfs count=\"1\">",
            "<xf numFmtId=\"0\" fontId=\"1\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyFont=\"1\"/>",
            "</cellXfs>",
            "<cellStyles count=\"1\">",
            "<cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/>",
            "</cellStyles>",
            "<dxfs count=\"0\"/>",
            "<tableStyles count=\"0\" defaultTableStyle=\"TableStyleMedium2\" defaultPivotStyle=\"PivotStyleLight16\"/>",
            "<extLst>",
            "<ext uri=\"{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}\" ",
            "xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\">",
            "<x14:slicerStyles defaultSlicerStyle=\"SlicerStyleLight1\"/>",
            "</ext>",
            "<ext uri=\"{9260A510-F301-46a8-8635-F512D64BE5F5}\" ",
            "xmlns:x15=\"http://schemas.microsoft.com/office/spreadsheetml/2010/11/main\">",
            "<x15:timelineStyles defaultTimelineStyle=\"TimeSlicerStyleLight1\"/>",
            "</ext>",
            "</extLst>",
            "</styleSheet>"
        ]).


-spec add_shared_strings(Xlsx :: #xlsx{}, SharedStrings :: map()) -> {ok, #xlsx{}}.
add_shared_strings(Xlsx, SharedStrings) ->
    Size = maps:size(SharedStrings),

    xlsx_util:write(Xlsx, "xl/sharedStrings.xml",
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<sst xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" count=\"", integer_to_list(Size), "\">",
            [
                ["<si><t>", z_html:escape(z_convert:to_list(Str)), "</t></si>"]
                || {Str, _Seq} <- lists:keysort(2, maps:to_list(SharedStrings))
            ],
            "</sst>"
        ]).
