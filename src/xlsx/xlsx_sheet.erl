-module(xlsx_sheet).

-export([encode_sheet/2]).

%%--------------------------------------------------------------------
%% @doc
%% generate sheet.xml for xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_sheet(Rows :: [iodata()], SharedStrings :: map()) -> {Sheet :: [iodata()], SharedStrings :: map()}.
encode_sheet(Rows, SharedStrings) ->
    Dimension = dimension(Rows),

    {RowStrings, UpdatedSharedStrings} = lists:foldl(
        fun(
            {Idx, Row},
            {
                AccRowStrings,
                AccSharedStrings
            }
        ) ->
            I = integer_to_list(Idx),
            {ColStrings, UpdatedAccSharedStrings} = lists:foldl(
                fun({Col, Cell}, {AccColStrings, AccAccSharedStrings}) ->
                    {{Kind, Content}, UpdatedAccAccSharedStrings} = encode(Cell, AccAccSharedStrings),

                    {
                            AccColStrings ++
                            [
                                "<c r=\"", column(Col), I, "\" t=\"", atom_to_list(Kind), "\">",
                                Content,
                                "</c>"
                            ],
                        UpdatedAccAccSharedStrings
                    }
                end,
                {[], AccSharedStrings},
                lists:zip(lists:seq(0, length(Row) - 1), Row)
            ),

            {
                    AccRowStrings ++ ["<row r=\"", I, "\">", ColStrings, "</row>"],
                UpdatedAccSharedStrings
            }
        end,
        {[], SharedStrings},
        lists:zip(lists:seq(1, length(Rows)), Rows)
    ),

    {
        [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n",
            "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
            "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" ",
            "xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" ",
            "mc:Ignorable=\"x14ac xr xr2 xr3\" ",
            "xmlns:x14ac=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac\" ",
            "xmlns:xr=\"http://schemas.microsoft.com/office/spreadsheetml/2014/revision\" ",
            "xmlns:xr2=\"http://schemas.microsoft.com/office/spreadsheetml/2015/revision2\" ",
            "xmlns:xr3=\"http://schemas.microsoft.com/office/spreadsheetml/2016/revision3\" ",
            "xr:uid=\"{00000000-0001-0000-0000-000000000000}\">",
            "<dimension ref=\"", Dimension, "\"/>",
            "<sheetViews>",
            "<sheetView tabSelected=\"1\" workbookViewId=\"0\"/>",
            "</sheetViews>",
            "<sheetData>",
            RowStrings,
            "</sheetData></worksheet>"
        ],
        UpdatedSharedStrings
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================


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
    [(N) + $A | Acc];
column(N, Acc) ->
    column(N div 26 - 1, [(N rem 26) + $A | Acc]).


%%--------------------------------------------------------------------
%% @doc
%% get Dimension of Rows
%%
%% @end
%%--------------------------------------------------------------------
dimension(Rows) ->
    [H | _T] = Rows,
    [
        "A1:",
        column(length(H)),
        integer_to_list(length(Rows))
    ].


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
    binary(),

    SharedStrings :: map()
) -> {
    {b, Value :: iodata()} |
    {n, Value :: iodata()} |
    {s, Value :: iodata()},

    UpdatedSharedStrings :: map()
}.
encode(true, SharedStrings) ->
    {{b, "<v>1</v>"}, SharedStrings};
encode(false, SharedStrings) ->
    {{b, "<v>0</v>"}, SharedStrings};
encode(I, SharedStrings) when is_integer(I) ->
    {{n, ["<v>", integer_to_list(I), "</v>"]}, SharedStrings};
encode(F, SharedStrings) when is_float(F) ->
    {{n, ["<v>", float_to_list(F), "</v>"]}, SharedStrings};
encode({{Year, Month, Day}, {_, _, _}}, SharedStrings) ->
    {{d,
        ["<v>", integer_to_list(Year), "-", integer_to_list(Month), "-", integer_to_list(Day), "</v>"]
    }, SharedStrings};
encode(Str, SharedStrings) ->
    case maps:get(Str, SharedStrings, undefined) of
        undefined ->
            Size = maps:size(SharedStrings),
            {
                {s, ["<v>", integer_to_list(Size), "</v>"]},
                SharedStrings#{
                    Str => Size
                }
            };
        Size ->
            {
                {s, ["<v>", integer_to_list(Size), "</v>"]},
                SharedStrings
            }
    end.
