%% -*- coding: latin-1 -*-
%% -------------------------------------------------------------------------
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%% @doc Check that msgstr uses the same tags, tag nesting, and tag
%% attributes as msgid - deviations will often (but not always) indicate an
%% error.

-module(gettext_validate_bad_html).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3
	]).

%% ----------------------------------------------------------------------------
-define(IS_CHAR(Char),
	is_integer(Char), ((Char >= 0) andalso (Char =< 255))).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    bad_html.

heading() ->
    "Changes in html".

%% ----------------------------------------------------------------------------
check({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    %% convert to ehtml
    Oehtml = gettext_yaws_html:h2e(OriginalFormatStr),
    Tehtml = gettext_yaws_html:h2e(TranslatedFormatStr),
    LocalAcc = look_for_bad_tags(Oehtml, Tehtml, [],
				 {OriginalFormatStr, TranslatedFormatStr}),

    %% check if any errors/warings should be ignored
    F = fun(E, Acc2) ->
		gettext_validate:do_ignore(
		  Ignores,
		  {bad_html, OriginalFormatStr, TranslatedFormatStr},
		  E,
		  Acc2)
	end,
    lists:foldl(F, [], LocalAcc) ++ Acc.



%% Note: only the first error of each branch is reported.
%% Note: see yaws.pdf (at http://yaws.hyber.org/) for Ehtml specification

%% -------------
%% process Body or similar list of tag / non-tag elements
%% Note: is_tuple(...) => is_ehtml_tag(...)

%% all [EHTML] content processed
look_for_bad_tags([], [], Acc, _) -> Acc;

%% [EHTML] - E is a tag
look_for_bad_tags([E|_], [], Acc, Info) when is_tuple(E) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning',
      "Tag is missing from the translation (this may be a error)",
      {original,    OriginalFormatStr},
      {translation, TranslatedFormatStr},
      {tag, E}} | Acc];

%% [EHTML] - E isn't a tag - ensure that this hold true for R
look_for_bad_tags([E|R], [], Acc, Info) ->
    Acc2 = look_for_bad_tags(E, [], Acc, Info),
    look_for_bad_tags(R, [], Acc2, Info);

%% [EHTML] - E is a tag
look_for_bad_tags([], [E|_], Acc, Info) when is_tuple(E) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning',
      "Tag has been added to translation (this may be ok)",
      {original,    OriginalFormatStr},
      {translation, TranslatedFormatStr},
      {tag, E}} | Acc];

%% [EHTML] - E isn't a tag - ensure that this hold true for R
look_for_bad_tags([], [E|R], Acc, Info) ->
    Acc2 = look_for_bad_tags([], E, Acc, Info),
    look_for_bad_tags([], R, Acc2, Info);

%% [EHTML] - both trees are non-empty - check first entry for error
look_for_bad_tags(Ehtml1, Ehtml2, Acc, Info)
  when is_list(Ehtml1), is_list(Ehtml2) ->
    Acc2 = look_for_bad_tags(hd(Ehtml1), hd(Ehtml2), Acc, Info),
    look_for_bad_tags(tl(Ehtml1), tl(Ehtml2), Acc2, Info);


%% -------------
%% {Tag, Attrs, Body} - tag and attrs match
look_for_bad_tags({Tag, Attrs, Body1}, {Tag, Attrs, Body2}, Acc, Info)
  when is_atom(Tag), is_list(Attrs) ->
    look_for_bad_tags(Body1, Body2, Acc, Info);
%% {Tag, Attrs, Body} - tag or attrs differ
look_for_bad_tags({Tag1, Attrs1, _}, {Tag2, Attrs2, _}, Acc, Info)
  when is_atom(Tag1), is_list(Attrs1), is_atom(Tag2), is_list(Attrs2) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning',
      "Tag or tag attributes differ, even though the html structure "
      "appears to be the same.",
      {original,    OriginalFormatStr},
      {translation, TranslatedFormatStr},
      {original_tag, Tag1},
      {translation_tag, Tag2},
      {original_attrs, Attrs1},
      {translation_attrs, Attrs2}} | Acc];

%% {Tag, Attrs} - tag and attrs match
look_for_bad_tags({Tag, Attrs}, {Tag, Attrs}, Acc, _)
  when is_atom(Tag), is_list(Attrs) -> Acc;
%% {Tag, Attrs} - tag or attrs differ
look_for_bad_tags({Tag1, Attrs1}, {Tag2, Attrs2}, Acc, Info)
  when is_atom(Tag1), is_list(Attrs1), is_atom(Tag2), is_list(Attrs2) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning',
      "Tag or tag attributes differ, even though the html structure "
      "appears to be the same.",
      {original,    OriginalFormatStr},
      {translation, TranslatedFormatStr},
      {original_tag, Tag1},
      {translation_tag, Tag2},
      {original_attrs, Attrs1},
      {translation_attrs, Attrs2}} | Acc];

%% {Tag} - tags match
look_for_bad_tags({Tag}, {Tag}, Acc, _) when is_atom(Tag) -> Acc;
%% {Tag} - tags differ
look_for_bad_tags({Tag1}, {Tag2}, Acc, Info)
  when is_atom(Tag1), is_atom(Tag2) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning',
      "Tags differ, even though the html structure appears to be the same.",
      {original,    OriginalFormatStr},
      {translation, TranslatedFormatStr},
      {original_tag, Tag1},
      {translation_tag, Tag2}} | Acc];


%% -------------
%% binary() - text no ehtml check needed
look_for_bad_tags(Bin1, Bin2, Acc, _)
  when is_binary(Bin1), is_binary(Bin2) -> Acc;
look_for_bad_tags(Bin1, [], Acc, _)
  when is_binary(Bin1) -> Acc;
look_for_bad_tags([], Bin2, Acc, _)
  when is_binary(Bin2) -> Acc;

%% char() - text no ehtml check needed
look_for_bad_tags(Char1, Char2, Acc, _)
  when ?IS_CHAR(Char1), ?IS_CHAR(Char2) -> Acc;
look_for_bad_tags(Char1, [], Acc, _)
  when ?IS_CHAR(Char1) -> Acc;
look_for_bad_tags([], Char2, Acc, _)
  when ?IS_CHAR(Char2) -> Acc;


%% -------------
%% ehtml trees differ
look_for_bad_tags(E1, E2, Acc, Info) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning',
      "Html tag nesting structure differs.",
      {original,    OriginalFormatStr},
      {translation, TranslatedFormatStr},
      {original_tree, E1},
      {translation_tree, E2}} | Acc].
