%% This is file `imsart-number.bst'
%%
%% The last changes added on 31\07\2009
%%
%% You are free to use this style file as you see fit, provided 
%% that you do not make changes to the file. 
%% If you DO make changes, you are required to rename this file.
%%
%% It may be distributed under the terms of the LaTeX Project Public
%% License, as described in lppl.txt in the base LaTeX distribution.
%% Either version 1.0 or, at your option, any later version.
%%
%% TeX programming: Linas Stonys, VTeX, Lithuania, lstonys@vtex.lt 
%%
%% This bibtex style produces structured bibliography (.bbl) file which could be
%% converted back into the database (.bib) format.
%% bibtex program must be invoked with the maximum allocation of memory (-w switch)
%%
%% This bibtex style could be used only with the imsart package 
%% starting with version from 2009/08/13


ENTRY
  { address
    author
    booktitle
    chapter
    doi
    edition
    editor
    howpublished
    institution
    isbn
    journal
    key
    month
    mrnumber
    note
    number
    organization
    pages   
    publisher
    school
    series
    src
    sortkey
    title
    type
    volume
    year
    url
    id
  }
  {}
   { label extra.label sort.label short.list }

INTEGERS { output.state before.all mid.sentence after.sentence
           after.block between.elements }

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
  #5 'between.elements :=
}

STRINGS { s t bother citetype}
STRINGS { longest.label last.label  }

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}

FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}


FUNCTION {output.nonnull}
{ 's :=
  output.state mid.sentence =
    { " " * write$ newline$}
    { output.state after.block =
        { add.period$ write$
          newline$
         % "\newblock " write$
        }
        { output.state between.elements =
            { ", " * write$ }
            { output.state before.all =
                'write$
                { add.period$ " " * write$ newline$}
              if$
            }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output}
{ duplicate$ empty$
    'pop$
    {output.nonnull}
  if$
}

FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    {output.nonnull}
  if$
}



FUNCTION {insert.comma}
{ output.state before.all =
    'skip$
    { between.elements 'output.state := }
  if$
}
FUNCTION {new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
        'skip$
        { after.sentence 'output.state :=}
      if$
    }
  if$
}


FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}


%%%%%%%%%%%%%%%% springer publisher
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% formating bbl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION {btitle}
{ duplicate$ empty$
    { pop$ "" }
    { "\btitle{" swap$ * "}" * }
  if$
}
INTEGERS {sk}
FUNCTION {is.num}
{ chr.to.int$
  duplicate$ "0" chr.to.int$ < not
  swap$ "9" chr.to.int$ > not and
}

FUNCTION {byear}
{ duplicate$ empty$
    { pop$ "" }
    { "\byear{" swap$ * "}" * }
  if$
}

FUNCTION {bctitle}
{ duplicate$ empty$
    { pop$ "" }
    { "\bctitle{" swap$ * "} " * }
  if$
}

FUNCTION {binstitute}
{ duplicate$ empty$
    { pop$ "" }
    { "\binstitute{" swap$ * "}" * }
  if$
}
FUNCTION {bsnm}
{ duplicate$ empty$
    { pop$ "" }
    { "\bsnm{" swap$ * "}" * }
  if$
}
FUNCTION {bpublisher}
{ duplicate$ empty$
    { pop$ "" }
    { bother "other" =
		 			'skip$
		 			{"\bpublisher{" swap$ * "}" * }
			if$
    }
  if$
}

FUNCTION {bedition}
{ duplicate$ empty$
    { pop$ "" }
    {	bother "other" =
		     'skip$ 
    		 {"\bedition{" swap$ * "}" * }
			if$ 
		}	
  if$
}
FUNCTION {bvolume}
{ duplicate$ empty$
    { pop$ "" }
    { "\bvolume{" swap$ * "}" * }
  if$
}
FUNCTION {betal}
{ duplicate$ empty$
    { pop$ "" }
    { "\betal{" swap$ * "}" * }
  if$
}
FUNCTION {binits}
{ duplicate$ empty$
    { pop$ "" }
    { "\binits{" swap$ * "}" * }
  if$
}
FUNCTION {bfnm}
{ duplicate$ empty$
    { pop$ "" }
    { "\bfnm{" swap$ * "}" * }
  if$
}
FUNCTION {bbtitle}
{ duplicate$ empty$
    { pop$ "" }
    { "\bbooktitle{" swap$ * "}" * }
  if$
}


FUNCTION {bparticle}
{ duplicate$ empty$
    { pop$ "" }
    { "\bparticle{" swap$ * "}" * }
  if$
}
FUNCTION {bsuffix}
{ duplicate$ empty$
    { pop$ "" }
    { "\bsuffix{" swap$ * "}" * }
  if$
}


FUNCTION {bjournal}
{ duplicate$ empty$
    { pop$ "" }
    { "\bjournal{" swap$ * "}" * }
  if$
}
%FUNCTION {bauthor}
%{ "\bauthor{" swap$ * "}" * }
%%%%%%%%%%%%%

FUNCTION {format.address}
{address empty$
		{ "" }
 		{ bother "other" =
		 			{address}
  		    {"\baddress{"  address * "}" *}
			if$
 		}
  if$
}
FUNCTION {format.organization}
{organization empty$
		{ "" }
		{organization bpublisher}
  if$
}

FUNCTION {format.org}
{organization empty$
		{ "" }
		{bother "other" =
		     {organization } 
    		 {"\borganization{" organization * "}" * } 
			if$
			add.period$
		}	
  if$
}




FUNCTION {format.pub.address}
{ publisher empty$
    { "" }
    { address empty$
        {publisher bpublisher}
        {publisher bpublisher ", " * format.address * add.period$}
      if$
    }
  if$
}
STRINGS { v l f j b ff}
INTEGERS { nameptr namesleft numnames}
FUNCTION {format.author.editor}
{ 'b :=
   	b "editor" =
   	b "beditor" =
   	b "editor2" =
   	or or
			{editor 's :=}
			{author 's :=}
		if$

  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    {	 s nameptr "{vv~}" format.name$ bparticle 'v := 
       s nameptr "{ll}" format.name$ bsnm 'l :=   
    	s nameptr "{f.}" format.name$ binits  'f := 
    	s nameptr "{ff}" format.name$ bfnm  'ff := 
     	s nameptr "{ jj}" format.name$ bsuffix 'j := 
 l  "\bsnm{others}" =
  {  "et~al." betal * }
  {
   	b "editor2" =
   		{j empty$
     		{"\beditor{" * ff * f * "~" * v * l * "}" * j 't := }
		 		{"\beditor{" * ff * f * "~" * v * l * j * "}" * "" 't :=}
		   if$
		  } 
		  'skip$
		if$
   	b "editor" =
   		{j empty$
		 		{"\beditor{" * v * l * ",~" * ff * f * "}" * j  't :=}
		 		{"\beditor{" * v * l * ",~" * ff * f * j * "}" * "" 't :=}
		   if$
		  } 
		  'skip$
		if$
   	b "author" =
   		{j empty$
		 		{"\bauthor{" * v * l * ",~" * ff * f * "}" * j  't :=}
		 		{"\bauthor{" * v * l * ",~" * ff * f * j * "}" * "" 't :=}
		   
		   if$
		  }
		  'skip$
		if$
	} 
 if$
		 	namesleft #0 >
        { namesleft #1 >
            { namesleft #2 =
    		        	{" \AND " * t * }
    		        	{", " * t * }
		          if$
           	}
            {""}
          if$
        }
        't
      
      if$
      nameptr #1 + 'nameptr := 
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {format.springer.names}
{ 's :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr "{vv~}{ll}{, f{.}.}{~jj}" format.name$ 't :=
    
      nameptr #1 >
        { namesleft #1 >
            { ", " * t * }
            { numnames #1 >
                { ", " * }
                'skip$
              if$
              t "others" =
                { " et~al." * }
                { "" * t * }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}



FUNCTION {format.editors}
{ editor empty$
    { "" }
    { bother "other" =
      	{"beditor" format.author.editor}
    		{"editor" format.author.editor}
			if$
			editor num.names$ #1 >
        { ", eds." * }
        { ", ed." * }
      if$
    }
  if$
}
FUNCTION {format.authors}
{ author empty$
    { "" }
 		{ bother "other" =
		 			{"bauthor" format.author.editor}
		 			{"author" format.author.editor}
			if$
 		}
  if$
}

FUNCTION {make.editors}
{ editor empty$
    { "" }
    { "("  
			bother "other" =
      	{author empty$
		      	{"beditor" format.author.editor}
		    		{editor format.springer.names}
					if$
      	}
    		{"editor2" format.author.editor}
			if$
						 *
      editor num.names$ #1 >
        { ", eds.)" * }
        { ", ed.)" * }
      if$
    }
  if$
}




FUNCTION {cite.author}
{ author empty$
	{""}
	{author #1 "{vv~}{ll}" format.name$
  author num.names$ duplicate$
  #3 >
    { pop$ " {\it et~al.}" * }
    { #3 =
        { author #3 "{vv~}{ll}{ jj}{ f}" format.name$ "others" =
            { " {\it et~al.}"  * }
            { ", " * author #2 "{vv~}{ll}" format.name$ * 
             " and " * author #3 "{vv~}{ll}" format.name$ *  
            }
          if$
        }
		    {author num.names$ #2 =
    		        {author #2 "{vv~}{ll}{ jj}{ f}" format.name$ "others" =
            				{ " {\it et~al.}"  * }
     		    				{ " and " * author #2 "{vv~}{ll}" format.name$ * }
     		    			if$
     		    		}
     		    		'skip$
        	 if$
        }
      if$
    }
  if$
  }
 if$ 
}


FUNCTION {check.year}
{year empty$
		{""}
		{year}
	if$	
}

FUNCTION {output.bibitem}
{ newline$
	  	"\bibitem{" write$ 
	  			cite$ write$
  		"}" write$
  		newline$
  		""
  before.all 'output.state :=
}
INTEGERS { multiresult char.num k }

 
FUNCTION {string.to.integer}
{ 't :=
    t text.length$ 'k :=
  #1 'char.num :=
      { t char.num #1 substring$ 's :=
	 	  s is.num
	 	  s "." =
	 	  or 
	 	  char.num k = not
	 	  and
	 	 	}
	 	 {char.num #1 + 'char.num :=}
  while$
  char.num #1 - 'char.num :=
  t #1 char.num substring$ 
}

FUNCTION {check.mrnumber}
{mrnumber 't :=
  ""
    { t empty$ not }
    { t #1 #2 substring$ "\#" = 
			{ t #1 #2 substring$ *
	 			 t #2 global.max$ substring$ 't :=
			}
			{
			  t #1 #1 substring$ "#" = 
					{ "\#" *
		 					 t #2 global.max$ substring$ 't :=
					}
				{ t #1 #1 substring$ *
	 			 t #2 global.max$ substring$ 't :=
				}
     	 if$
      }
      if$
    }
  while$
}

FUNCTION {format.mrnumber}
{ mrnumber empty$
    { "" }
    {	bother "other" =
		     {check.mrnumber} 
		     {"\bmrnumber{" check.mrnumber * "}" * }
			if$
    }
  if$
}
INTEGERS { found }
FUNCTION {cut.info}
{ 't :=
 #0 'found := 
 "" 's :=
 "" 'v :=
    { t empty$ not }
    { 
      t #1 #5 substring$ "info:" = 
			  { 
    		  t #6 global.max$ substring$ 't :=
    		  #1 'found := 
          "}" 'v :=
			  }
			  'skip$
      if$

      t #1 #1 substring$ "/" = 
      #1 found =
      and
			  { 
    		  t #2 global.max$ substring$ 't :=
    		  s "={" * 's :=
    		  #0 'found := 
           
			  }
			  'skip$
      if$   

      t #1 #2 substring$ ", " = 
      v "}" =
      and
			  { 
    		  t #2 global.max$ substring$ 't :=
    		  s "}," * 's :=
			  }
			  'skip$
      if$   
            s t #1 #1 substring$ * 's :=  
	 			    t #2 global.max$ substring$ 't :=
    }
  while$
s v *
}

FUNCTION {format.id}
{ id empty$
    { "" }
    {	bother "other" =
		     { "" } 
		     {"\bid{" id cut.info * "}" * }
			if$
    }
  if$
}
FUNCTION {format.journal}
{ journal empty$
    { "" }
    {	bother "other" =
		     {journal "t" change.case$} 
    		 {"\bjournal{" journal  * "}" * } 
			if$
    }
  if$
}
FUNCTION {title.sep}
{ type$ "article" =
	type$ "incollection" =
	type$ "inproceedings" =
	type$ "booklet" =
	type$ "unpublished" =
	or or or or
		'add.period$
		'skip$
	if$

	type$ "inbook" =
	type$ "manual" =
	or
	edition empty$ not
	and 
		{"," *}
		{
			edition empty$ 
			series empty$ not
			and
				'add.period$
				'skip$
			if$	
		}
	if$

	type$ "book" =
		{
			edition empty$ not
			series empty$ not
			and
				{"," *}
				{
				 edition empty$ 
				 series empty$ not
				 and
						'add.period$
						'skip$
				 if$		
				}
			if$
		}
		'skip$
	if$
	
}

FUNCTION {format.title}
{ title empty$
    { "" }
    {	bother "other" =
		     {title} 
    		 {title  btitle} 
			if$
			title.sep
    }
  if$
}
FUNCTION {format.year}
{ year empty$
    {""}
    {	bother "other" =
    		{"(" year  extra.label * * ")." *}  
    		{"(" year byear * ")." *}  
			if$
    }
    if$
}

FUNCTION {format.month}
{ month empty$
    {""}
		{ "\bmonth{" month  * "}" *}
	if$
}
FUNCTION {format.type}
{ type empty$
    {""}
		{ "\btype{" type * "}" *}
	if$
}

FUNCTION {formatpatent.date}
{ year empty$
    { month empty$
        { "" }
        { "there's a month but no year in " cite$ * warning$}
         
      if$
    }
    { month empty$
       { year byear}
        {format.month " " * year byear *}
      if$
    }
  if$
}
FUNCTION {format.btitle}
{ title bbtitle
}

FUNCTION {bcomment}
{ duplicate$ empty$
    { pop$ "" }
    { bother "other" =
		 			'skip$
		 			{"\bnote{" swap$ * "}" * }
			if$
    }
  if$
}


FUNCTION {format.note}
{ note empty$
    {""}
    {	note bcomment}
  if$
}

FUNCTION {add.note}
{ note empty$
    {""}
    {	note }
  if$
}
FUNCTION {format.chapter}
{chapter empty$
	{""}
	{"\bchapter{" chapter * "}" * 	}
if$	
}


FUNCTION {chap.type}
{chapter empty$
	{""}
	{	type empty$
			{ "Chapter "}
			{ type "Section" = 
	 				{	"Section "}
					{ "Chapter "}
				if$	
			}
		if$
		 "\bchapter{" chapter * "}" * *
	}
if$	
}
								
  
FUNCTION {chap.note}
{	format.note output 
     new.sentence
%	chap.type output
 %    new.sentence
}

FUNCTION {tie.or.space.connect}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ * *
}
FUNCTION {no.space.connect}
{ duplicate$ text.length$ #3 <
    { "" }
    { "" }
  if$
  swap$ * *
}


%%%%%%%%%%%%%%%%%%%%%%%%%% patent names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION {either.or.check}
{ empty$
    'pop$
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}
FUNCTION {format.series}
{ series empty$
	{ "" } 
    {	bother "other" =
		     {series} 
    		 {"\bseries{" series * "}" * } 
			if$
    }
  if$
}
FUNCTION {format.edition}
{ edition empty$
    { "" }
    { output.state mid.sentence =
        { edition bedition "l" change.case$ }
        { edition bedition "t" change.case$ }
      if$
      " ed." *
    }
  if$
}


FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult 
}
FUNCTION {format.isbn}
{ isbn empty$
    { "" }
    {	bother "other" =
   				 {"\bisbn{" isbn * "}" *}
   				 {"\bisbn{" isbn * "}" *}
			if$
		}  
  if$
}

FUNCTION {format.school}
{ school empty$
		{""}
    {"\bschool{" school * "}" * } 
  if$
}

FUNCTION {format.institution}
{ institution empty$
	{""}
    {	bother "other" =
		     {institution} 
    		 {"\binstitute{" institution * "}" * } 
			if$
    }
  if$
}
FUNCTION {format.howpublished}
{ howpublished empty$
	{""}
    {	bother "other" =
		     {howpublished} 
    		 {"\bhowpublished{" howpublished * "}" * } 
			if$
    }
  if$
}

FUNCTION {format.vol}
{ volume empty$
	{ number empty$ 
			{""}
 		 {"\banumber{" number  * "}" * } 
		if$
	}
    {	bother "other" =
		     {volume} 
    		 {"\bvolume{" volume  * "}" * } 
			if$
    }
  if$
}

FUNCTION {note.presented}
 {note #1 #9 substring$ "presented" = 
 	note #1 #9 substring$ "Presented" = 
 or}
 
%  if$
%}

FUNCTION {format.url}
{ url empty$
    { "" }
    {	bother "other" =
    		 {"Available at \burl{" url * "}" * } 
    		 {"Available at \burl{" url * "}" * } 
			if$
    }
  if$
}

FUNCTION {format.doi}
{ doi empty$
		{""}
		 {bother "other" =
		     {"\bdoi{" doi  * "}" * } 
    		 {"\bdoi{" doi * "}" * } 
			if$
			 
    }
  if$
}

FUNCTION {format.ppages}
{ pages  empty$ 
		{""}
		{ bother "other" =
		     {pages} 
    		 {"\bpages{" pages * "}" * } 
   	 if$
   	 add.period$
    }
  if$
}


FUNCTION {format.booktitle}
{ booktitle empty$
    { "" }
    {	bother "other" =
		     {"In " booktitle *} 
    		 {"In \bbooktitle{" booktitle * "}" *} 
			if$
			type$ "incollection" =
				{	series empty$
					volume empty$
					and
						'skip$
						{ editor empty$ series empty$ not and
						   {"." *} 
						   {"," *} 
							if$
						}
					if$	
				}
				'skip$
			if$
				
			type$ "inproceedings" =
				{	series empty$
					editor empty$
%					volume empty$ not
					and 
						'skip$
						{editor empty$
							  {"." *}
							  'skip$
						 if$ 
						}
					if$	
				}
				'skip$
			if$	
    }
  if$
}

FUNCTION {format.tr.number}
{ type empty$
    { "\btype{Technical Report}" }
    { "\btype{" type * "} report" * }
  if$

  number empty$
		 'skip$
    { " No. \bnumber{" number * "}" * * }
  if$

%  type empty$ not 
%  number empty$
	institution empty$ not
    	{"," *}
    	'skip$
 	if$

}
FUNCTION {patent.number}
{number empty$
		 'skip$
    { "\bnumber{" number * "}" * }
  if$
}
FUNCTION {format.number}
{number empty$
		{""}
    {"\bnumber{" number * "}" * } 
  if$
}
INTEGERS { len }

FUNCTION {chop.word}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}   
 

FUNCTION {bcomment.note}
{note empty$
'skip$
 {note  bcomment output add.period$}
 if$
}

FUNCTION {empty.misc.check}
{ author empty$ title empty$ howpublished empty$
  month empty$ year empty$ note empty$
  and and and and and
    { "all relevant fields are empty in " cite$ * warning$ }
    'skip$
  if$
}

FUNCTION {format.thesis.type}
{ type empty$
    'skip$
    { pop$
      "\btype" type * "}" *
    }
  if$
}


FUNCTION {element.type}
{	bother "other" =
  		{"\begin{botherref}" write$ newline$}
  		{"\begin{" write$
					"b" type$ *  		write$
  			"}["  write$
  					src empty$
  						{"author" write$}
  						{src write$ }
  					if$
  			"]" write$ newline$
  		}
  if$
}
FUNCTION {end.element.type}
{write$ newline$
	bother "other" =
  		{"\end{botherref}" write$ newline$}
  		{"\end{" write$
					"b" type$ *  		write$
  			"}" write$ newline$
  		}
  if$
  "\endbibitem" write$ newline$
}
FUNCTION {same.end}
{ add.period$
  format.url output
	new.sentence
  format.doi output
	new.sentence
  format.mrnumber output
	new.sentence
	format.id       output
 }

FUNCTION {article}
{ output.bibitem
	"form" 'bother := 
element.type
	format.authors output.nonnull
	format.year output
	format.title "title" output.check
	format.journal  output 
	format.vol 			output 
  format.ppages 	output
  new.sentence
	bcomment.note
  same.end
end.element.type
}


FUNCTION {manual}
{ output.bibitem
	"form" 'bother := 
element.type
  author empty$
    { organization empty$
        'skip$
        { format.organization output.nonnull}
      if$
    }
    { format.authors output.nonnull}
  if$
    format.year output
  new.sentence
  format.title "title" output.check
  
  format.edition output
  author empty$
    { organization empty$
        'skip$
        { format.address output}
      if$
    }
    { format.organization output
      address empty$
         'skip$
         { insert.comma }
      if$
      format.address output
    }
  if$
 % new.sentence
	bcomment.note
  same.end
end.element.type
 }
 

FUNCTION {patent}
{  "form" 'bother := 
 output.bibitem
 element.type
  format.authors output.nonnull
  format.year output
  format.title "title" output.check
	patent.number output
  new.sentence
	bcomment.note
  same.end
  
end.element.type
}

FUNCTION { other } { patent }

FUNCTION {book}
{ output.bibitem
	"form" 'bother := 
element.type
  author empty$
    { format.editors output}
    { format.authors output.nonnull}
  if$
  format.year output
  format.title  output
	series empty$
	  {
		  format.vol output
		  edition empty$
    		'new.sentence
				'insert.comma	  
		  if$
			format.edition output
    	new.sentence
		}
		{
			format.edition output
	    new.sentence
  		format.series output
		  format.vol output
    	new.sentence
		}
	if$
%  format.ppages output
  format.pub.address output
	bcomment.note
  same.end
  
end.element.type
}

FUNCTION {booklet}
{  "form" 'bother := 
  output.bibitem
 element.type
  format.authors output.nonnull
  format.year  output
  format.title "title" output.check
  format.howpublished output
 			address empty$
				'new.sentence
    		'insert.comma
		  if$
	format.address output
	bcomment.note
  same.end
  
 end.element.type
}

FUNCTION {inbook}
{ output.bibitem
	"form" 'bother := 
element.type
  author empty$
    { format.editors output}
    { format.authors output.nonnull}
  if$
 format.year output
 format.title output
% make.editors output  
 format.edition output
 edition empty$
 	  'skip$
  	{new.sentence}
 if$ 	
 format.series output
 format.vol output
 format.chapter output
 format.ppages output
 new.sentence
 format.pub.address output
 chap.note %output
 new.sentence
 same.end
end.element.type
}

FUNCTION {incollection}
{ output.bibitem
	"form" 'bother := 
element.type
	format.authors output.nonnull
	format.year output
  format.title "title" output.check
	format.booktitle "booktitle" output.check
  format.edition output
  make.editors output 
  series empty$ editor empty$
  or
  	'skip$
  	'new.sentence
	if$
		  format.series output
     	format.vol output
     	format.chapter output
		 	format.ppages output
      format.pub.address output
	bcomment.note
  same.end
end.element.type
}

FUNCTION {inproceedings}
{ output.bibitem
	"form" 'bother := 
element.type
	format.authors output.nonnull
	format.year output
  format.title "title" output.check
  format.booktitle "booktitle" output.check
  make.editors output  
  editor empty$ series empty$
  or
  	'skip$
  	'new.sentence
  if$
	format.series output
 	format.vol output
 	format.ppages output
  new.sentence
					publisher empty$
          	{format.organization output}
          	{format.org output}
          if$
 %         new.sentence
  format.pub.address output
	bcomment.note
  same.end
end.element.type
}

FUNCTION {conference} { inproceedings }


FUNCTION {mastersthesis}
{  "form" 'bother := 
  output.bibitem
 element.type
  format.authors output.nonnull
  format.year  output
  format.title "title" output.check
  type empty$
     {"\btype{Master's thesis}" output.nonnull}
     {format.type output}
  if$     
  school empty$
     'skip$
     { insert.comma }
  if$
  format.school  output
  address empty$
     'skip$
     { insert.comma }
  if$
  format.address output
	bcomment.note
  same.end
  
 end.element.type
}

FUNCTION {misc}
{  "form" 'bother := 
  output.bibitem
 element.type
  format.authors output.nonnull
  format.year output
  format.title "title" output.check
  new.sentence
%  "other" 'bother := 
  format.howpublished output
%  "form" 'bother := 
 	bcomment.note
  same.end
 
 empty.misc.check
 end.element.type
}

FUNCTION {phdthesis}
{  "form" 'bother := 
  output.bibitem
 element.type
  format.authors output.nonnull
  format.year output
  format.title "title" output.check
  type empty$
     {"\btype{PhD thesis}" output.nonnull}
     {format.type output}
  if$     
  school empty$
     'skip$
     { insert.comma }
  if$
  format.school output
  address empty$
     'skip$
     { insert.comma }
  if$
  format.address output
	bcomment.note
  same.end 
  
 end.element.type
}

FUNCTION {proceedings}
{ output.bibitem
	"form" 'bother := 
element.type
   author empty$
    { editor empty$
   			  {organization binstitute "organization" output.check}
    			{ format.editors output.nonnull }
  		if$ 
  	}
    { format.authors output.nonnull}
  if$
  format.year output
  format.btitle "title" output.check
 	format.series output
	format.vol output
          new.sentence
  author empty$ editor empty$ and
  	'skip$        
  	{format.org output}
	if$
  format.pub.address output
  format.ppages output
	bcomment.note
  same.end
  
end.element.type
 
}

FUNCTION {techreport}
{  "form" 'bother := 
output.bibitem
element.type
  format.authors output.nonnull
  format.year output
  format.title "title" output.check
  format.tr.number output
	format.institution output
	new.sentence
  address empty$
     'skip$
     { insert.comma }
  if$
  format.address output
	bcomment.note
  same.end
  
end.element.type
}

FUNCTION {unpublished}
{  "form" 'bother := 
output.bibitem
element.type
  format.authors output.nonnull
  format.year output
  format.title "title" output.check
	bcomment.note
  same.end
end.element.type
}

FUNCTION {default.type} { unpublished }


READ

FUNCTION {sortify}
{ purify$
  "l" change.case$
}
FUNCTION {format.lab.names}
{ 's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr "{ vv }{ ll }{ f }{ jj }" format.name$ 't :=
      
       t sortify * "  " *
	     	  nameptr #1 + 'nameptr :=
  	    	namesleft #1 - 'namesleft :=
    }
  while$}

FUNCTION {author.key.label}
{ author empty$
    { key empty$
        { cite$ #1 #3 substring$ }
        'key
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {author.editor.key.label}
{ author empty$
    { editor empty$
        { key empty$
            { cite$ #1 #3 substring$ }
            'key
          if$
        }
        { editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {author.key.organization.label}
{ author empty$
    { key empty$
        { organization empty$
            { cite$ #1 #3 substring$ }
            { "The " #4 organization chop.word #3 text.prefix$ }
          if$
        }
        'key
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {editor.key.organization.label}
{ editor empty$
    { key empty$
        { organization empty$
            { cite$ #1 #3 substring$ }
            { "The " #4 organization chop.word #3 text.prefix$ }
          if$
        }
        'key
      if$
    }
    { editor format.lab.names }
  if$
}

FUNCTION {calc.short.authors}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.label
    { type$ "proceedings" =
        'editor.key.organization.label
        { type$ "manual" =
            'author.key.organization.label
            'author.key.label
          if$
        }
      if$
    }
  if$
  'short.list :=
}

FUNCTION {calc.label}
{ calc.short.authors
  short.list
  "("
  *
  year duplicate$ empty$
  short.list key field.or.null = or
     { pop$ "" }
     'skip$
  if$
  *
  'label :=
}

FUNCTION {sort.format.names}
{ 's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    {% nameptr #1 >
     %   { "   " * numnames int.to.str$ * "   " *}
     %   'skip$
     % if$
      s nameptr "{vv{ } }{ll{ }}{ff{ }}{jj{ }}" format.name$ 't :=
      nameptr numnames = 
      t "others" = 
      and
        { "et al" * }
        { t sortify * "  " *}
      if$
%		#3 numnames  <
%  			{#0 'namesleft :=
%  				 	"zzz   " *
%  			}    
%	      {
	     	  nameptr #1 + 'nameptr :=
  	    	namesleft #1 - 'namesleft :=
%  	    }
%  	  if$  	
    }
  while$
}

FUNCTION {sort.format.title}
{ 't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}

FUNCTION {author.sort}
{ author empty$
    { key empty$
        { "to sort, need author or key in " cite$ * warning$
          "zzz"
        }
        { key sortify }
      if$
    }
    { author sort.format.names }
  if$
}

FUNCTION {author.editor.sort}
{ author empty$
    { editor empty$
        { key empty$
            { "to sort, need author, editor, or key in " cite$ * warning$
              "z"
            }
            { key sortify }
          if$
        }
        { editor sort.format.names }
      if$
    }
    { author sort.format.names }
  if$
}

FUNCTION {author.organization.sort}
{ author empty$
    { organization empty$
        { key empty$
            { "to sort, need author, organization, or key in " cite$ * warning$
              "z"
            }
            { key sortify }
          if$
        }
        { "The " #4 organization chop.word sortify }
      if$
    }
    { author sort.format.names }
  if$
}

FUNCTION {editor.organization.sort}
{ editor empty$
    { organization empty$
        { key empty$
            { "to sort, need editor, organization, or key in " cite$ * warning$
              "z"
            }
            { key sortify }
          if$
        }
        { "The " #4 organization chop.word sortify }
      if$
    }
    { editor sort.format.names }
  if$
}

FUNCTION {presort}
{ calc.label
  label sortify
  "    "
  *
  type$ "book" =
  type$ "inbook" =
  or
 		{author empty$
   		'editor.organization.sort
      'author.organization.sort
     if$
    }
    { type$ "proceedings" =
      type$ "incollection" =
       or
    		{author empty$
        		'editor.organization.sort
            'author.organization.sort
        if$
        }
        { type$ "manual" =
            'author.organization.sort
            'author.sort
          if$
        }
      if$
    }
  if$
  "    "
  *
  year field.or.null sortify
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$ 'sort.label :=
%  sort.label top$
  sort.label *
  #1 entry.max$ substring$ 'sort.key$ :=
%  sort.key$ top$
}

ITERATE {presort}

SORT


INTEGERS { longest.label.width last.extra.num number.label }

FUNCTION {initialize.longest.label}
{ "" 'longest.label :=
  #0 int.to.chr$ 'last.label :=
  "" 'j :=
  #0 'longest.label.width :=
  #0 'last.extra.num :=
  #0 'number.label :=
}

FUNCTION {forward.pass}
{ last.label label =
    { last.extra.num #1 + 'last.extra.num :=
      last.extra.num int.to.chr$ 'extra.label :=
    }
    { "a" chr.to.int$ 'last.extra.num :=
      "" 'extra.label :=
      label 'last.label :=
    }
  if$
  number.label #1 + 'number.label :=
}

FUNCTION {reverse.pass}
{ j "b" =
    { "a" 'extra.label := }
    'skip$
  if$
  extra.label 'j :=
  label extra.label * 'label :=
}

EXECUTE {initialize.longest.label}

%ITERATE {forward.pass}

%REVERSE {reverse.pass}

FUNCTION {bib.sort.order}
{ sort.label  'sort.key$ :=
}

ITERATE {bib.sort.order}

SORT



FUNCTION {begin.bib}
{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\begin{thebibliography}{" number.label int.to.str$ * "}" * write$ newline$
}
EXECUTE {begin.bib}

EXECUTE {init.state.consts}


ITERATE {call.type$}

FUNCTION {end.bib}
{ newline$
 "\end{thebibliography}" write$ newline$
}

EXECUTE {end.bib}




