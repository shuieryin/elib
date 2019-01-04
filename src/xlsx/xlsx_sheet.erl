-module(xlsx_sheet).

-export([encode_sheet/1]).

%%--------------------------------------------------------------------
%% @doc
%% generate sheet.xml for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_sheet(Rows :: []) -> Sheet :: [string()].
encode_sheet(Rows) ->
    [
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
     "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
     "<sheetViews>",
     "<sheetView tabSelected=\"1\" workbookViewId=\"0\"/>",
     "</sheetViews>",
     "<sheetData>",
     [encode_row(Idx, Row)  || {Idx, Row} <- lists:zip(lists:seq(1, length(Rows)), Rows)],
     "</sheetData></worksheet>"
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% generate row format data for sheet.xml
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_row(Idx :: integer(), Row :: [any()]) -> RowData :: [any()].
encode_row(Idx, Row) ->
    I = integer_to_list(Idx),
    ["<row r=\"", I, "\">",
     [begin
          {Kind, Content, Style} = encode(Cell),
          ["<c r=\"", column(Col), I, "\" t=\"", atom_to_list(Kind), "\"",
           " s=\"", integer_to_list(Style), "\">", Content, "</c>"
          ]
      end || {Col, Cell} <- lists:zip(lists:seq(0, length(Row)-1), Row)],
     "</row>"].


%%--------------------------------------------------------------------
%% @doc
%% generate column number for row reference
%% Given 0-based column number return the Excel column name as list.
%%
%% @end
%%--------------------------------------------------------------------
-spec column(N :: integer()) -> ColName :: string().
column(N) ->
    column(N, []).

-spec column(N :: integer(), Acc :: [integer()]) -> ColName :: string().
column(N, Acc) when N < 26 ->
    [(N)+$A | Acc];
column(N, Acc) ->
    column(N div 26-1, [(N rem 26)+$A|Acc]).


%%--------------------------------------------------------------------
%% @doc
%% encode cell value to xlsx format
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(
    RawValue :: boolean() |
    integer() |
    float() |
    calendar:datetime() |
    {b, string()} |
    {i, string()} |
    {f, string()} |
    string() |
    binary()
) -> {b, Value :: string(), Type :: integer()} |
{n, Value :: string(), Type :: integer()} |
{inlineStr, Value :: string(), Type :: integer()}.
encode(true) ->
    {b, "<v>1</v>", 6};
encode(false) ->
    {b, "<v>0</v>", 6};
encode(I) when is_integer(I) ->
    {n, ["<v>", integer_to_list(I), "</v>"], 3};
encode(F) when is_float(F) ->
    {n, ["<v>", float_to_list(F), "</v>"], 4};
encode({{Year,Month,Day},{_,_,_}}) ->
    {n, ["<v>", Year, "-", Month, "-", Day ,"</v>"], 3};
encode({b, Str}) ->
    {inlineStr, ["<is><t>", z_html:escape(z_convert:to_list(Str)), "</t></is>"], 7};
encode({i, Str}) ->
    {inlineStr, ["<is><t>", z_html:escape(z_convert:to_list(Str)), "</t></is>"], 8};
encode({f, Str}) ->
    {inlineStr, ["<is><t>", z_html:escape(z_convert:to_list(Str)), "</t></is>"], 9};
encode(Str) ->
    {inlineStr, ["<is><t>", z_html:escape(z_convert:to_list(Str)), "</t></is>"], 5}.


%% 1 = time
%% 2 = date
%% 3 = int
%% 4 = float
%% 5 = string
%% 6 = bool

%
% ms_epoch() ->
%     calendar:datetime_to_gregorian_seconds({{1904, 1, 1}, {0, 0, 0}}).
    
