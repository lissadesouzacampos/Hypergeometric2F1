(* ::Package:: *)

BeginPackage["Hypergeometric2F1`"];


(* ::Section:: *)
(*Usage *)


	a::usage = "Parameter of hypergeometric equation: z(1-z)\!\(\*FractionBox[\(\*SuperscriptBox[\(d\), \(2\)] w\), SuperscriptBox[\(dz\), \(2\)]]\)+(c-(a+b+1)z)\!\(\*FractionBox[\(dw\), \(dz\)]\)-abw=0 .";
	b::usage = "Parameter of hypergeometric equation: z(1-z)\!\(\*FractionBox[\(\*SuperscriptBox[\(d\), \(2\)] w\), SuperscriptBox[\(dz\), \(2\)]]\)+(c-(a+b+1)z)\!\(\*FractionBox[\(dw\), \(dz\)]\)-abw=0.";
	c::usage = "Parameter of hypergeometric equation: z(1-z)\!\(\*FractionBox[\(\*SuperscriptBox[\(d\), \(2\)] w\), SuperscriptBox[\(dz\), \(2\)]]\)+(c-(a+b+1)z)\!\(\*FractionBox[\(dw\), \(dz\)]\)-abw=0.";	
	PossibleOptions::usage = "
		PossibleOptions[wi] prints the possible choices of 'sector', 'branch cut' and 'domain' (on the real line) of the hypergeometric solution wi, with i\[Element]{1,2,3,4,5,6}.
	";
	w1::usage = "
		w1[z] is a solution of the hypergeometric equation suitable at z=0. It accepts the following options.
		'form': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'sector': the sector of the z-complex plane on which w1 is analytic. 
		'branch cut': the branch cut of w1, which lies on the real line and is written as {endpoint1, endpoint2}.
		'domain': the domain on the real line on which w1 is analytic; it's written as {endpoint1, endpoint2}
		'verbose': if set to True, prints the corresponding values of 'form', 'sector', 'branch cut' and 'domain' of w1.
		To check with values of options are valid, use PossibleOptions[w1].
		
		w1[j,k,z] gives w1 written as a linear combination of wj and wk. One can choose j=3 and k=4, or j=5 and k=6. 
		It also accepts the following options.
		'form wj': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'form wk': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'verbose': if set to True, prints the corresponding values of 'form wj', 'form wk', 'domain wj', 'domain wk' and 'domain w1'.
	";
	w2::usage = "
		w2[z] is a solution of the hypergeometric equation suitable at z=0. It accepts the following options.
		'form': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'sector': the sector of the z-complex plane on which w2 is analytic. 
		'branch cut': the branch cut of w2, which lies on the real line and is written as {endpoint1, endpoint2}.
		'domain': the domain on the real line on which w2 is analytic; it's written as {endpoint1, endpoint2}
		'verbose': if set to True, prints the corresponding values of 'form', 'sector', 'branch cut' and 'domain' of w2.
		To check with values of options are valid, use PossibleOptions[w2].
		
		w2[j,k,z] gives w2 written as a linear combination of wj and wk. One can choose j=3 and k=4, or j=5 and k=6. 
		It also accepts the following options.
		'form wj': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'form wk': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'verbose': if set to True, prints the corresponding values of 'form wj', 'form wk', 'domain wj', 'domain wk' and 'domain w1'.
	";
	w3::usage = "
		w3[z] is a solution of the hypergeometric equation suitable at z=1. It accepts the following options.
		'form': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'sector': the sector of the z-complex plane on which w3 is analytic. 
		'branch cut': the branch cut of w3, which lies on the real line and is written as {endpoint1, endpoint2}.
		'domain': the domain on the real line on which w3 is analytic; it's written as {endpoint1, endpoint2}
		'verbose': if set to True, prints the corresponding values of 'form', 'sector', 'branch cut' and 'domain' of w2.
		To check with values of options are valid, use PossibleOptions[w3].
		
		w3[j,k,z] gives w3 written as a linear combination of wj and wk. One can choose j=1 and k=2, or j=5 and k=6. 
		It also accepts the following options.
		'form wj': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'form wk': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'verbose': if set to True, prints the corresponding values of 'form wj', 'form wk', 'domain wj', 'domain wk' and 'domain w1'.
	";
	w4::usage = "
		w4[z] is a solution of the hypergeometric equation suitable at z=1. It accepts the following options.
		'form': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'sector': the sector of the z-complex plane on which w4 is analytic. 
		'branch cut': the branch cut of w4, which lies on the real line and is written as {endpoint1, endpoint2}.
		'domain': the domain on the real line on which w4 is analytic; it's written as {endpoint1, endpoint2}
		'verbose': if set to True, prints the corresponding values of 'form', 'sector', 'branch cut' and 'domain' of w4.
		To check with values of options are valid, use PossibleOptions[w4].
		
		w4[j,k,z] gives w4 written as a linear combination of wj and wk. One can choose j=1 and k=2, or j=5 and k=6. 
		It also accepts the following options.
		'form wj': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'form wk': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'verbose': if set to True, prints the corresponding values of 'form wj', 'form wk', 'domain wj', 'domain wk' and 'domain w1'.
	";
	w5::usage = "
		w5[z] is a solution of the hypergeometric equation suitable at z=\[Infinity]. It accepts the following options.
		'form': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'sector': the sector of the z-complex plane on which w5 is analytic. 
		'branch cut': the branch cut of w5, which lies on the real line and is written as {endpoint1, endpoint2}.
		'domain': the domain on the real line on which w5 is analytic; it's written as {endpoint1, endpoint2}
		'verbose': if set to True, prints the corresponding values of 'form', 'sector', 'branch cut' and 'domain' of w2.
		To check with values of options are valid, use PossibleOptions[w5].
		
		w5[j,k,z] gives w5 written as a linear combination of wj and wk. One can choose j=1 and k=2, or j=3 and k=4. 
		It also accepts the following options.
		'form wj': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'form wk': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'verbose': if set to True, prints the corresponding values of 'form wj', 'form wk', 'domain wj', 'domain wk' and 'domain w1'.
	";
	w6::usage = "
		w6[z] is a solution of the hypergeometric equation suitable at z=\[Infinity]. It accepts the following options.
		'form': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'sector': the sector of the z-complex plane on which w6 is analytic. 
		'branch cut': the branch cut of w6, which lies on the real line and is written as {endpoint1, endpoint2}.
		'domain': the domain on the real line on which w6 is analytic; it's written as {endpoint1, endpoint2}
		'verbose': if set to True, prints the corresponding values of 'form', 'sector', 'branch cut' and 'domain' of w6.
		To check with values of options are valid, use PossibleOptions[w6].
		
		w6[j,k,z] gives w6 written as a linear combination of wj and wk. One can choose j=1 and k=2, or j=3 and k=4. 
		It also accepts the following options.
		'form wj': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'form wk': can be 1,2,3 or 4. Each gives the same solution written in a different form. 
		'verbose': if set to True, prints the corresponding values of 'form wj', 'form wk', 'domain wj', 'domain wk' and 'domain w1'.
	";


(* ::Section:: *)
(*Messages *)



(* TODO *)



(* ::Section:: *)
(*Definitions*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Hypergeometric functions*)


Options[w1]={
"form"->1,
"sector"-> "|ph(1-z)|<\[Pi]",
"branch cut"->{1,\[Infinity]},
"domain"->{-\[Infinity],1},
"verbose"->False
};
PossibleOptions[w1]:=(
CellPrint[TextCell[Row[{"The function w1 is set for: ", ExpressionCell[Highlighted[TableForm[{{  "Scenario","'sector'","'branch cut'","'domain on the real line'"},
{ "III","|ph(1-z)|<\[Pi]","{1,\[Infinity]}","{-\[Infinity],1}"}}],Background->LightBlue]], "\[NoBreak]\n\n 4 equivalent solutions are selected by the assignment of 'form' into 1, 2, 3 or 4. \n One can also choose 'verbose' \[Rule] True to check when the solution is well-defined and equivalent to the other forms."}],"Text","FontSize"->12,CellFrame->True]]
)
w1[z_,OptionsPattern[]]:=(
Module[{form},
(* For sector \[Rule] |ph(1-z)|<\[Pi] && |ph(z)|<\[Pi], branch cut \[Rule] {{-\[Infinity],0},{1,\[Infinity]}}, domain =={0,1}, one can choose:
form[2]=(z-1)^(c-a-b) Hypergeometric2F1[c-a,c-b,c,z];
	form[3]=(z-1)^-a Hypergeometric2F1[a,c-b,c,z/(z-1)];
	form[4]=(z-1)^-b Hypergeometric2F1[b,c-a,c,z/(z-1)];
but then, even though they are equal to each other, they are not equal to form \[Rule] 1 *)

If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form \[Rule] `1` , sector \[Rule] |ph(1-z)|<\[Pi], branch cut \[Rule] {1,\[Infinity]}, domain \[Rule] {-\[Infinity],1}}",OptionValue["form"]]]];
form[1]=Hypergeometric2F1[a,b,c,z];
form[2]=(1-z)^(c-a-b)*Hypergeometric2F1[c-a,c-b,c,z];
form[3]=(1-z)^-a*Hypergeometric2F1[a,c-b,c,z/(z-1)];
form[4]=(1-z)^-b*Hypergeometric2F1[b,c-a,c,z/(z-1)];

form[OptionValue["form"]]
]);


Options[w2]={
"form"->1,
"sector"-> "|ph(z)|<\[Pi]",
"branch cut"->{-\[Infinity],0},
"domain"->{0,1},
"verbose"->False
};
PossibleOptions[w2]:=(
CellPrint[TextCell[Row[{"The function w2 is set with default options: ", ExpressionCell[Options[w2]], "\n\n You can also choose amongst:      ", ExpressionCell[Highlighted[TableForm[{{ "Scenario","'sector'","'branch cut'","'domain'"},
{ "I","|ph(z)|<\[Pi]","{-\[Infinity],0}","{0,1}"},
{ "III","|ph(1-z)|<\[Pi]","{1,\[Infinity]}","{-\[Infinity],0},{0,1},{-\[Infinity],1}"}}],Background->LightBlue]], "\[NoBreak]\n\n Each scenario has 4 solutions, selected by the assignment of 'form' into 1, 2, 3 or 4. \n The option 'domain' specifies the domain on the real line of validity and equivalence of the 4 solutions. Solutions from different scenarios are linearly independent, but not equivalent. \n One can also choose 'verbose' \[Rule] True to check when the solution is well-defined and equivalent to the other forms."}],"Text","FontSize"->12,CellFrame->True]]
)
w2[z_,OptionsPattern[]]:=(
Module[{aux,form,case},
Do[aux[i]=0,{i,Range[1,4]}];
If[OptionValue["sector"]=="|ph(z)|<\[Pi]"&&OptionValue["branch cut"]=={-\[Infinity],0}&&OptionValue["domain"]=={0,1},
aux[1]=z^(1-c);
aux[2]=z^(1-c) (1-z)^(c-a-b);
aux[3]=z^(1-c) (1-z)^(c-a-1);
aux[4]=z^(1-c) (1-z)^(c-b-1);
case=1,
(*(* these are well-defined, but not equal for z>1*)
 aux[1]=z^(1-c);
aux[2]=z^(1-c)(z-1)^(c-a-b);
aux[3]=z^(1-c)(z-1)^(c-a-1);
aux[4]=z^(1-c)(z-1)^(c-b-1);
;*)
If[OptionValue["sector"]=="|ph(1-z)|<\[Pi]"||OptionValue["branch cut"]=={1,\[Infinity]}|| Cases[{{0,1},{-\[Infinity],0},{-\[Infinity],1}},OptionValue["domain"]]=!={},
aux[1]=(-z)^(1-c);
aux[2]=(-z)^(1-c) (1-z)^(c-a-b);
aux[3]=(-z)^(1-c) (1-z)^(c-a-1);
aux[4]=(-z)^(1-c) (1-z)^(c-b-1);
case=2
]];

If[OptionValue["verbose"]==True,If[case==1, Print[Framed@StringForm["{form \[Rule] `1` , sector \[Rule] |ph(z)|<\[Pi], branch cut \[Rule] {-\[Infinity],0}, domain \[Rule] {0,1}}",OptionValue["form"]]], Print[Framed@StringForm["{form \[Rule] `1` , sector \[Rule] |ph(1-z)|<\[Pi], branch cut \[Rule] {1,\[Infinity]}, domain \[Rule] {-\[Infinity],1}}",OptionValue["form"]]]]];
form[1]=aux[1]*Hypergeometric2F1[a-c+1,b-c+1,2-c,z];
form[2]=aux[2]*Hypergeometric2F1[1-a,1-b,2-c,z];
form[3]=aux[3]*Hypergeometric2F1[a-c+1,1-b,2-c,z/(z-1)];
form[4]=aux[4]*Hypergeometric2F1[1-a,b-c+1,2-c,z/(z-1)];

form[OptionValue["form"]]]
);


Options[w3]={
"form"->1,
"sector"-> "|ph(z)|<\[Pi]",
"branch cut"->{-\[Infinity],0},
"domain"->{0,\[Infinity]},
"verbose"->False
};
PossibleOptions[w3]:= (
CellPrint[TextCell[Row[{"The function w3 is set for: ", ExpressionCell[Highlighted[TableForm[{{ "Scenario","'sector'","'branch cut'","'domain'"},
{ "I","|ph(z)|<\[Pi]","{-\[Infinity],0}","{0,\[Infinity]}"}}],Background->LightBlue]], "\[NoBreak]\n\n 4 equivalent solutions are selected by the assignment of 'form' into 1, 2, 3 or 4.  \n One can also choose 'verbose' \[Rule] True to check when the solution is well-defined and equivalent to the other forms."}],"Text","FontSize"->12,CellFrame->True]]
)
w3[z_,OptionsPattern[]]:=(
Module[{form},
If[OptionValue["domain"][[1]]<0 || OptionValue["domain"][[2]]<0,Return["Check PossibleOptions[w3]. The chosen domain is not a possible option. Either the function cannot be defined there, or it can, but one of the other equivalences can't."]];
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form\[Rule]`1` , sector\[Rule]|ph(z)|<\[Pi], branch cut\[Rule]{-\[Infinity],0}, domain\[Rule]{0,\[Infinity]}}",OptionValue["form"]]]];
form[1]=Hypergeometric2F1[a,b,a+b-c+1,1-z];
form[2]=z^(1-c) Hypergeometric2F1[a-c+1,b-c+1,a+b-c+1,1-z];
form[3]=z^-a*Hypergeometric2F1[a,a-c+1,a+b-c+1,1-1/z];
form[4]=z^-b*Hypergeometric2F1[b,b-c+1,a+b-c+1,1-1/z];

form[OptionValue["form"]]]);


Options[w4]={
"form"->1,
"sector"-> "|ph(1-z)|<\[Pi] && |ph(z)|<\[Pi]",
"branch cut"->{{-\[Infinity],0},{1,\[Infinity]}},
"domain"->{0,1},
"verbose"->False
};
PossibleOptions[w4]:=(
CellPrint[TextCell[Row[{"The function w4 is set with default options: ", ExpressionCell[Options[w4]], "\n\n You can also choose amongst:      ", ExpressionCell[Highlighted[TableForm[{{ "Scenario","'sector'","'branch cut'","'domain'"},
{ "IV","|ph(1-z)|<\[Pi] && |ph(z)|<\[Pi]","{-\[Infinity],0},{1,\[Infinity]}","{0,1}"},
{ "V",                      "",                                        "{-\[Infinity],1}",                 "{1,\[Infinity]}"}}],Background->LightBlue]], "\[NoBreak]\n\n Each scenario has 4 equivalent solutions, selected by the assignment of 'form' into 1, 2, 3 or 4. Solutions from different scenarios are linearly independent, but not equivalent.  \n One can also choose 'verbose' \[Rule] True to check when the solution is well-defined and equivalent to the other forms."}],"Text","FontSize"->12,CellFrame->True]]
)
w4[z_,OptionsPattern[]]:=(
If[(OptionValue["domain"][[1]]<0 || OptionValue["domain"][[2]]<0) || (OptionValue["domain"][[1]]<1 && OptionValue["domain"][[2]]>1)|| (OptionValue["domain"][[1]]>1 && OptionValue["domain"][[2]]<1) ,Return["Check PossibleOptions[w4]. The chosen domain is not a possible option. Either the function cannot be defined there, or it can, but one of the other equivalences can't."]];
Module[{aux,form,case},
aux[OptionValue["form"]]=0;
If[OptionValue["sector"]=="|ph(1-z)|<\[Pi] && |ph(z)|<\[Pi]"&&OptionValue["branch cut"]=={{-\[Infinity],0},{1,\[Infinity]}}&&OptionValue["domain"][[1]]<= 1&&OptionValue["domain"][[2]]<= 1,
(*these are also equal for z>1 if we choose a side of the branch*)
aux[1]=(1-z)^(c-a-b);
aux[2]=z^(1-c) (1-z)^(c-a-b);
aux[3]=z^(a-c) (1-z)^(c-a-b);
aux[4]=z^(b-c) (1-z)^(c-a-b);
case=1
];
If[OptionValue["branch cut"]=={-\[Infinity],1} || (OptionValue["domain"][[1]]>= 1&&OptionValue["domain"][[2]]>=1),
aux[1]=(z-1)^(c-a-b);
aux[2]=z^(1-c) (z-1)^(c-a-b);
aux[3]=z^(a-c) (z-1)^(c-a-b);
aux[4]=z^(b-c) (z-1)^(c-a-b);
case=2
];
If[OptionValue["verbose"]==True,If[case==1, Print[Framed@StringForm["{form\[Rule]`1` , sector \[Rule] |ph(1-z)|<\[Pi] && |ph(z)|<\[Pi], branch cut \[Rule] {{-\[Infinity],0},{1,\[Infinity]}}, domain\[Rule]{0,1}}",OptionValue["form"]]], Print[Framed@StringForm["{form\[Rule]`1` , branch cut\[Rule]{-\[Infinity],1}, domain\[Rule]{1,\[Infinity]}}",OptionValue["form"]]]]];
form[1]=aux[1]*Hypergeometric2F1[c-a,c-b,c-a-b+1,1-z];
form[2]=aux[2]*Hypergeometric2F1[1-a,1-b,c-a-b+1,1-z];
form[3]=aux[3]*Hypergeometric2F1[1-a,c-a,c-a-b+1,1-1/z];
form[4]=aux[4]*Hypergeometric2F1[1-b,c-b,c-a-b+1,1-1/z];

If[aux[OptionValue["form"]]=!= 0,form[OptionValue["form"]],Print["Something went wrong, maybe you have chosen incompatible conditions. Please check documentation."]]
]);


Options[w5]={
"form"->1,
"sector"->"|ph(-z)|<\[Pi]",
"branch cut"->{0,\[Infinity]},
"domain"->{-\[Infinity],0},
"verbose"->False
};
PossibleOptions[w5]:=(
CellPrint[TextCell[Row[{"The function w5 is set with default options: ", ExpressionCell[Options[w5]], "\n\n You can also choose amongst:      ", ExpressionCell[Highlighted[TableForm[{{ "Scenario",         "'sector'",                                         "'branch cut'",     "'domain'"},
{ "II",                    "|ph(-z)|<\[Pi]",                                   "{0,\[Infinity]}",                  "{-\[Infinity],0}"},
{ "V",                      " ",                                                          "{-\[Infinity],1}",                 "{1,\[Infinity]}"}}],Background->LightBlue]], "\[NoBreak]\n\n Each scenario has 4 equivalent solutions, selected by the assignment of 'form' into 1, 2, 3 or 4. Solutions from different scenarios are linearly independent, but not equivalent.  \n One can also choose 'verbose' \[Rule] True to check when the solution is well-defined and equivalent to the other forms."}],"Text","FontSize"->12,CellFrame->True]]
)
w5[z_,OptionsPattern[]]:=(
Module[{aux,form,case},
Do[aux[i]=0,{i,Range[1,4]}];
If[OptionValue["sector"]=="|ph(-z)|<\[Pi]"&&OptionValue["branch cut"]=={0,\[Infinity]} && OptionValue["domain"]=={-\[Infinity],0},
(* These are also equal for z>1 if we choose one side of the branch)*)
aux[1]=(-z)^-a;(*branch 0,\[Infinity]*)
aux[2]=(-z)^(b-c) (1-z)^(c-a-b);(*branch 0,\[Infinity]*)
aux[3]=(1-z)^-a;(*branch 0,\[Infinity]*)
aux[4]=(-z)^(1-c) (1-z)^(c-a-1) ;(*branch 0,\[Infinity]*)
case=1,
If[OptionValue["branch cut"]=={-\[Infinity],1} || OptionValue["domain"]=={1,\[Infinity]},
aux[1]=z^-a; (*branch -\[Infinity],1*)
aux[2]=z^(b-c) (z-1)^(c-a-b); (*branch -\[Infinity],1*)
aux[3]=(z-1)^-a;(*branch -\[Infinity],1*)
aux[4]=z^(1-c) (z-1)^(c-a-1) ;
case=2(*branch -\[Infinity],1*)
(*, If[OptionValue["NIST"]\[Equal]True,
(* The expressions from NIST are equal for z<0, but we have to choose one side of the branch cut *)
aux[1]=\[ExponentialE]^(\[ImaginaryI] \[Pi] a)z^-a; (*branch -\[Infinity],1*) 
aux[2]=\[ExponentialE]^(\[ImaginaryI] \[Pi](c-b))z^(b-c)(1-z)^(c-a-b);(*branch Real line *) 
aux[3]=(1-z)^-a;(*branch 0,\[Infinity]*) 
aux[4]=\[ExponentialE]^(\[ImaginaryI] \[Pi](c-1))z^(1-c)(1-z)^(c-a-1) (*branch Real line*)
]*)]];
(* 
(* These are options that I didn't need *)
aux[1]=\[ExponentialE]^(-\[ImaginaryI] \[Pi] a)z^-a; (*branch -\[Infinity],1*)
aux[2]=z^(b-c)(1-z)^(c-a-b); (*branch ReaL line*)
aux[2]=\[ExponentialE]^(\[ImaginaryI] \[Pi](b-c))z^(b-c)(1-z)^(c-a-b);(*branch Real line *)
aux[2]=(-z)^(b-c)(z-1)^(c-a-b); (*branch Real line*)
aux[2]=\[ExponentialE]^(\[ImaginaryI] \[Pi](c-b))z^(b-c)(z-1)^(c-a-b); (*branch -\[Infinity],1*)
aux[2]=\[ExponentialE]^(\[ImaginaryI] \[Pi](b-c))z^(b-c)(z-1)^(c-a-b); (*branch -\[Infinity],1*)
aux[3]=\[ExponentialE]^(-\[ImaginaryI] \[Pi] a)(1-z)^-a; (*branch 0,\[Infinity]*)
aux[3]=\[ExponentialE]^(\[ImaginaryI] \[Pi] a)(1-z)^-a;(*branch 0,\[Infinity]*)
aux[3]=\[ExponentialE]^(-\[ImaginaryI] \[Pi] a)(z-1)^-a; (*branch -\[Infinity],1*)
aux[3]=\[ExponentialE]^(\[ImaginaryI] \[Pi] a)(z-1)^-a;(*branch -\[Infinity],1*)
aux[4]=z^(1-c)(1-z)^(c-a-1) ;(*branch Real line*)
aux[4]=(-z)^(1-c)(z-1)^(c-a-1) ;(*branch Real line*)
aux[4]=\[ExponentialE]^(\[ImaginaryI] \[Pi](c-1))z^(1-c)(z-1)^(c-a-1) ;(*branch -\[Infinity],1*)
aux[4]=\[ExponentialE]^(\[ImaginaryI] \[Pi](1-c))z^(1-c)(z-1)^(c-a-1); (*branch -\[Infinity],1*)
aux[4]=\[ExponentialE]^(\[ImaginaryI] \[Pi](1-c))z^(1-c)(1-z)^(c-a-1) ;(*branch Real line*) *)
If[aux[OptionValue["form"]]==0,Print["Something went wrong, maybe you have chosen incompatible conditions. Please check documentation."]];
If[OptionValue["verbose"]==True,If[case==1, Print[Framed@StringForm["{form\[Rule]`1` , sector\[Rule]|ph(-z)|<\[Pi], branch cut\[Rule]{1,\[Infinity]}, domain\[Rule]{-\[Infinity],0}}",OptionValue["form"]]], Print[Framed@StringForm["{form\[Rule]`1` , branch cut\[Rule]{-\[Infinity],1}, domain\[Rule]{1,\[Infinity]}}",OptionValue["form"]]]]];
form[1]=aux[1]*Hypergeometric2F1[a,a-c+1,a-b+1,1/z];
form[2]=aux[2]*Hypergeometric2F1[1-b,c-b,a-b+1,1/z];
form[3]=aux[3]*Hypergeometric2F1[a,c-b,a-b+1,1/(1-z)];
form[4]=aux[4]*Hypergeometric2F1[1-b,a-c+1,a-b+1,1/(1-z)];

form[OptionValue["form"]]]);


Options[w6]={
"form"->1,
"sector"->"|ph(-z)|<\[Pi]",
"branch cut"->{0,\[Infinity]},
"domain"->{-\[Infinity],0},
"verbose"->False
};
PossibleOptions[w6]:=(
CellPrint[TextCell[Row[{"The function w6 is set with default options: ", ExpressionCell[Options[w6]], "\n\n You can also choose amongst:      ", ExpressionCell[Highlighted[TableForm[{{ "Scenario",         "'sector'",                                         "'branch cut'",     "'domain'"},
{"II",                    "|ph(-z)|<\[Pi]",                                   "{0,\[Infinity]}",                  "{-\[Infinity],0}"},
{ "V",                            " ",                                                          "{-\[Infinity],1}",                 "{1,\[Infinity]}"}}],Background->LightBlue]], "\[NoBreak]\n\n Each scenario has 4 equivalent solutions, selected by the assignment of 'form' into 1, 2, 3 or 4. Solutions from different scenarios are linearly independent, but not equivalent.  \n One can also choose 'verbose' \[Rule] True to check when the solution is well-defined and equivalent to the other forms."}],"Text","FontSize"->12,CellFrame->True]]
)
w6[z_,OptionsPattern[]]:=(
Module[{aux,form,case},
If[OptionValue["sector"]=="|ph(-z)|<\[Pi]"&&OptionValue["branch cut"]=={0,\[Infinity]} && OptionValue["domain"]=={-\[Infinity],0},
(* These are also equal for z>1 if we choose one side of the branch)*)
aux[1]=(-z)^-b;(*branch 0,\[Infinity]*)
aux[2]=(-z)^(a-c) (1-z)^(c-b-a);(*branch 0,\[Infinity]*)
aux[3]=(1-z)^-b;(*branch 0,\[Infinity]*)
aux[4]=(-z)^(1-c) (1-z)^(c-b-1) ;(*branch 0,\[Infinity]*)
case=1,
If[ OptionValue["branch cut"]=={-\[Infinity],1} || OptionValue["domain"]=={1,\[Infinity]},
aux[1]=z^-b; (*branch -\[Infinity],1*)
aux[2]=z^(a-c) (z-1)^(c-b-a); (*branch -\[Infinity],1*)
aux[3]=(z-1)^-b;(*branch -\[Infinity],1*)
aux[4]=z^(1-c) (z-1)^(c-b-1) ;(*branch -\[Infinity],1*)
case=2
]];
If[aux[OptionValue["form"]]==0,Print["Something went wrong, maybe you have chosen incompatible conditions. Please check documentation."]];
If[OptionValue["verbose"]==True,If[case==1, Print[Framed@StringForm["{form\[Rule]`1` , sector\[Rule]|ph(-z)|<\[Pi], branch cut\[Rule]{1,\[Infinity]}, domain\[Rule]{-\[Infinity],0}}",OptionValue["form"]]], Print[Framed@StringForm["{form\[Rule]`1` , branch cut\[Rule]{-\[Infinity],1}, domain\[Rule]{1,\[Infinity]}}",OptionValue["form"]]]]];
form[1]=aux[1]*Hypergeometric2F1[b,b-c+1,b-a+1,1/z];
form[2]=aux[2]*Hypergeometric2F1[1-a,c-a,b-a+1,1/z];
form[3]=aux[3]*Hypergeometric2F1[b,c-a,b-a+1,1/(1-z)];
form[4]=aux[4]*Hypergeometric2F1[1-a,b-c+1,b-a+1,1/(1-z)];

form[OptionValue["form"]]]);


(* ::Subsection:: *)
(*Connection formulas*)


Options[Connection12]={
"form w1"->1,
"form w2"->1,
"verbose"->False
};
w3[1,2,z_,OptionsPattern[Connection12]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w1 \[Rule] `1`, form w2 \[Rule] `2`, domain w1 \[Rule] {-\[Infinity],1},  domain w2 \[Rule] {0,1}, domain w3 \[Rule] {0,\[Infinity]}}",OptionValue["form w1"],OptionValue["form w2"]]]];
(Gamma[1-c]Gamma[a+b-c+1])/(Gamma[a-c+1]Gamma[b-c+1]) w1[z,"form"->OptionValue["form w1"]]+(Gamma[c-1]Gamma[a+b-c+1])/(Gamma[a]Gamma[b]) w2[z, "form"->OptionValue["form w2"]]
);
w4[1,2,z_,OptionsPattern[Connection12]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w1 \[Rule] `1`, form w2 \[Rule] `2`, domain w1 \[Rule] {-\[Infinity],1},  domain w2 \[Rule] {-\[Infinity],1}, domain w4 \[Rule] {0,1}}",OptionValue["form w1"],OptionValue["form w2"]]]];
(Gamma[1-c]Gamma[c-a-b+1])/(Gamma[1-a]Gamma[1-b]) w1[z,"form"->OptionValue["form w1"]]+(Gamma[c-1]Gamma[c-a-b+1])/(Gamma[c-a]Gamma[c-b]) w2[z, "form"->OptionValue["form w2"]]
);
w5[1,2,z_,OptionsPattern[Connection12]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w1 \[Rule] `1`, form w2 \[Rule] `2`, domain w1 \[Rule] {-\[Infinity],1},  domain w2 \[Rule] {-\[Infinity],1}, domain w5 \[Rule] {-\[Infinity],0}}",OptionValue["form w1"],OptionValue["form w2"]]]];
(Gamma[1-c]Gamma[a-b+1])/(Gamma[a-c+1]Gamma[1-b]) w1[z,"form"->OptionValue["form w1"]]+(Gamma[c-1]Gamma[a-b+1])/(Gamma[a]Gamma[c-b]) w2[z, "form"->OptionValue["form w2"],"domain"->{-\[Infinity],1}]
);
w6[1,2,z_,OptionsPattern[Connection12]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w1 \[Rule] `1`, form w2 \[Rule] `2` , domain w1 \[Rule] {-\[Infinity],1},  domain w2 \[Rule] {-\[Infinity],1}, domain w6 \[Rule] {-\[Infinity],0}}",OptionValue["form w1"],OptionValue["form w2"]]]];
(Gamma[1-c]Gamma[b-a+1])/(Gamma[b-c+1]Gamma[1-a]) w1[z,"form"->OptionValue["form w1"]]+(Gamma[c-1]Gamma[b-a+1])/(Gamma[b]Gamma[c-a]) w2[z, "form"->OptionValue["form w2"],"domain"->{-\[Infinity],1}]);


Options[Connection34]={
"form w3"->1,
"form w4"->1,
"verbose"->False
};
w1[3,4,z_,OptionsPattern[Connection34]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w3 \[Rule] `1`, form w4 \[Rule] `2` , domain w3 \[Rule] {0,\[Infinity]},  domain w4 \[Rule] {0,1}, domain w1 \[Rule] {-\[Infinity],1}}",OptionValue["form w3"],OptionValue["form w4"]]]];
(Gamma[c]Gamma[c-a-b])/(Gamma[c-a]Gamma[c-b]) w3[z,"form"->OptionValue["form w3"]]+(Gamma[c]Gamma[a+b-c])/(Gamma[a]Gamma[b]) w4[z, "form"->OptionValue["form w4"]]
);
w2[3,4,z_,OptionsPattern[Connection34]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w3 \[Rule] `1`, form w4 \[Rule] `2` , domain w3 \[Rule] {0,\[Infinity]},  domain w4 \[Rule] {0,1}, domain w2 \[Rule] {0,1}}",OptionValue["form w3"],OptionValue["form w4"]]]];
(Gamma[2-c]Gamma[c-a-b])/(Gamma[1-a]Gamma[1-b]) w3[z,"form"->OptionValue["form w3"]] (Gamma[2-c]Gamma[a+b-c])/(Gamma[a-c+1]Gamma[b-c+1]) w4[z, "form"->OptionValue["form w4"]]
);

w5[3,4,z_,OptionsPattern[Connection34]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w3 \[Rule] `1`, form w4 \[Rule] `2`, domain w3 \[Rule] {0,\[Infinity]}, domain w4 \[Rule]{1,\[Infinity]}, domain w5 \[Rule] {1,\[Infinity]}}",OptionValue["form w3"],OptionValue["form w4"]]]];
(Gamma[a-b+1]Gamma[c-a-b])/(Gamma[1-b]Gamma[c-b]) w3[z,"form"->OptionValue["form w3"]]+(Gamma[a-b+1]Gamma[a+b-c])/(Gamma[a]Gamma[a-c+1]) w4[z, "form"->OptionValue["form w4"],"domain"-> {1,\[Infinity]}]
);
w6[3,4,z_,OptionsPattern[Connection34]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w3 \[Rule] `1`, form w4 \[Rule] `2`, domain w3 \[Rule] {0,\[Infinity]}, domain w4 \[Rule] {1,\[Infinity]}, domain w6 \[Rule] {1,\[Infinity]}}",OptionValue["form w3"],OptionValue["form w4"]]]];
(Gamma[b-a+1]Gamma[c-a-b])/(Gamma[1-a]Gamma[c-a]) w3[z,"form"->OptionValue["form w3"]]+(Gamma[b-a+1]Gamma[a+b-c])/(Gamma[b]Gamma[b-c+1]) w4[z, "form"->OptionValue["form w4"], "domain"->{1,\[Infinity]}]
);


Options[Connection56]={
"form w5"->1,
"form w6"->1,
"verbose"->False
};
w1[5,6,z_,OptionsPattern[Connection56]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w5 \[Rule] `1`, form w6 \[Rule] `2`, domain w5 \[Rule] {-\[Infinity],0},  domain w6 \[Rule] {-\[Infinity],0}, domain w1 \[Rule] {-\[Infinity],0}}",OptionValue["form w5"],OptionValue["form w6"]]]];(Gamma[c]Gamma[b-a])/(Gamma[b]Gamma[c-a]) w5[z,"form"->OptionValue["form w5"]]+(Gamma[c]Gamma[a-b])/(Gamma[a]Gamma[c-b]) w6[z, "form"->OptionValue["form w6"]]
);
w2[5,6,z_,OptionsPattern[Connection56]]:=(
If[OptionValue["verbose"]==True, 
Print[Framed@StringForm["{form w5 \[Rule] `1`, form w6 \[Rule] `2`, domain w5 \[Rule] {-\[Infinity],0},  domain w6 \[Rule] {-\[Infinity],0}, domain w2 \[Rule] {-\[Infinity],0}}",OptionValue["form w5"],OptionValue["form w6"]]]];

(Gamma[2-c]Gamma[b-a])/(Gamma[1-a]Gamma[b-c+1]) w5[z,"form"->OptionValue["form w5"]]+(Gamma[2-c]Gamma[a-b])/(Gamma[1-b]Gamma[a-c+1]) w6[z, "form"->OptionValue["form w6"]]
);

w3[5,6,z_,OptionsPattern[Connection56]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w5 \[Rule] `1`, form w6 \[Rule] `2`, domain w5 \[Rule] {1,\[Infinity]},  domain w6 \[Rule] {1,\[Infinity]}, domain w3 \[Rule] {0,\[Infinity]}}",OptionValue["form w5"],OptionValue["form w6"]]]];(Gamma[a+b-c+1]Gamma[b-a])/(Gamma[b]Gamma[b-c+1]) w5[z,"form"->OptionValue["form w5"],"domain"->{1,\[Infinity]}]+(Gamma[a+b-c+1]Gamma[a-b])/(Gamma[a]Gamma[a-c+1]) w6[z, "form"->OptionValue["form w6"],"domain"->{1,\[Infinity]}]
);
w4[5,6,z_,OptionsPattern[Connection56]]:=(
If[OptionValue["verbose"]==True, Print[Framed@StringForm["{form w5 \[Rule] `1`, form w6 \[Rule] `2`, domain w5 \[Rule] {1,\[Infinity]},  domain w6 \[Rule] {1,\[Infinity]}, domain w4 \[Rule] {1,\[Infinity]}}",OptionValue["form w5"],OptionValue["form w6"]]]];(Gamma[c-a-b+1]Gamma[b-a])/(Gamma[1-a]Gamma[c-a]) w5[z,"form"->OptionValue["form w5"],"domain"->{1,\[Infinity]}]+(Gamma[c-a-b+1]Gamma[a-b])/(Gamma[1-b]Gamma[c-b]) w6[z, "form"->OptionValue["form w6"],"domain"->{1,\[Infinity]}]
);


(* ::Subsection:: *)
(**)


	

	End[];
	
EndPackage[];

