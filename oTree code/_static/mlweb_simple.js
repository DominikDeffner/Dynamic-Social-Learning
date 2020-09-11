//
// 	MouselabWEB  
//                

//     this script contains functions to display MouselabWEB content
// 								
//       v 0.97.1, July 2004     
//
//     (c) 2003-2004 Martijn C. Willemsen and Eric J. Johnson 
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


dtNewDate = new Date(); 
starttime = dtNewDate.getTime(); // abs. starttime of experiment

// set vars for delay 
prevtime = 0; 	// memory in timefunction to compensate for delay
dtime=0;		 
prevCell = -1; 	// delay lag memory (-1 means first cell is not delayable 
loaded = false; // flag to test whether page has been loaded 9is set to true in reorder
boxOpen = false; // flag to test whether box is already open (using in showCont and hideCont)

transpImg = new Image()
tempImg = new Image()   


// default values
mlweb_outtype="XML";
mlweb_fname=0;
masterCond = 1;  // number of master conditions
randomOrder = false; // force randomize of counterbalancing order
btnColor = "red";
subject="1";

// get source transparant image
transpImg.src="transp.gif"

function abc_num(str)
{
out=str.toUpperCase().charCodeAt(0)-65;
return out
}

function fac(x)
{
// Faculty: x!=x(x-1)...1
var outp=1;
for (var i=1; i<=x; i++)
{outp=outp*i}
return outp
}

function CountBal(subjnr, num)
{
// counterbalance based on subj number. 
// first subject is 0
// Num is number of options to counterbalance
// (number of orders is Num!)

var numOrd=fac(num);
start = subjnr - numOrd*Math.floor((subjnr-1)/numOrd)

orderstr=""
for (var i=0;i<num;i++)
{orderstr+=i.toString()}

outstr=""
for (var i=num; i>0; i--)
{
var den=fac(i-1);
pos = Math.floor((start-1)/den)+1
outstr+=orderstr.charAt(pos-1)+","
orderstr = orderstr.substring(0,pos-1)+orderstr.substr(pos)
start=start-(pos-1)*den
}
outstr=outstr.substr(0,outstr.length-1)
return outstr.split(",")
}

function ExpMatrix(M)
{ // expand data matrices
var Mrows=M.split("`");

var outM = new Array();
for (rowcount=0;rowcount<Mrows.length;rowcount++)
	{
	outM[rowcount]=Mrows[rowcount].split("^")
	}
return outM;
}

function ExpRow(M)
{ // expand data vectors

var outM = new Array();
outM = M.split("^") 
return outM;
}

function timefunction(event,name,value) {
// Record proc data in form element
mlweb_form=document.forms[mlweb_fname].elements['procdata']

	dtNewDate = new Date();
	eventtime = dtNewDate.getTime();
  	var curtime = eventtime-starttime-dtime;  // dtime is to compensate for delay time (failed openings have negative time!
//	if (prevtime>curtime) {curtime=prevtime;} else {prevtime=curtime}; // check with previous event time: if smaller, then delay was not finished: set curtime to prevtime so event has duration 0;

	dtime=0; // reset dtime
  	if (mlweb_outtype=="x")
	   	{
	   	var str="<eventblock><event>"+event+"</event><name>"+name+"</name><value>"+value+"</value><time>"+curtime+"</time></eventblock>";
  		var headerstr="<?xml version=1.0?>"
		}
		else 
		{
		var str="\""+event+"\",\""+name+"\",\""+value+"\",\""+curtime+"\"\n"
		var headerstr="\"event\",\"name\",\"value\",\"time\"\n"
		};
		
	if(mlweb_form.value=='') 
		{
		mlweb_form.value=headerstr;
  		}
 	mlweb_form.value+=str;
	
	loaded = true;
	
return true;
}

// convert event to eventdata and call save function
function RecordEventData(objActionElement, objEvent)
	{
	var strName, strEventType, strFormValue;
	strName = objActionElement.name;
	strFormValue = (objActionElement.value) ? objActionElement.value : "";
	strEventType = objEvent.type;

	//call timefunction 
	timefunction(strEventType,strName, strFormValue)
	return false;
	}
	
function checkForm(formHandle)
{
if (chkchoice=="nobuttons") {return true;}
if (chkchoice==true) {timefunction('submit','submit','succeeded');return true} else {timefunction('submit','submit','failed');return false};
}

function objElem(name,value)
{
this.name=name
this.value=value
}

function ShowCont(fieldname , contents , objEvent)
{
if (!loaded) {return;} // do not open boxes when page is loading

boxOpen = true; //set flag to show box is open

//var row = abc_num(fieldname);
//var col = parseInt(fieldname.substr(1));

thisElem = new objElem;
// check if open cell should be recorded
//if ((statecont[RowOut[row]][ColOut[col]]=="0") & !(recOpenCells)) {return;}

// retrieve tagname and txt for this cell
thisElem.name = fieldname;
thisElem.value = contents;

RecordEventData(thisElem, objEvent);

if (document.getElementById)  
	{
	// IE6/NS6>/Mozilla
	HandleTxt = document.getElementById(fieldname+"_td");
	HandleBox = document.getElementById(fieldname+"_box");
	}
	else if (document.all)
	{
	//IE4/5
	HandleTxt=eval("document.all['"+fieldname+"_td"+"']");
	HandleBox=eval("document.all['"+fieldname+"_box"+"']");
	}

// delay

//currCell = -1; 
//for (var i=0;i<Dlist.length;i++)
//	{if (tagcont[RowOut[row]][ColOut[col]]==Dlist[i]) {currCell=i;break;}}

//if ((prevCell!=-1)&(currCell!=-1)) {dtime = DTimes[currCell][prevCell];} else {dtime=0}; 
//prevCell = currCell;

//HandleTxt.style.visibility='visible';HandleBox.style.visibility='hidden';

delay=window.setTimeout("HandleTxt.style.visibility='visible';HandleBox.style.visibility='hidden';",dtime)  //make image transparant

}

function HideCont(fieldname,objEvent)
{
if (!loaded) {return;} // do not open boxes when page is loading
if (!boxOpen) {return;} // do not close boxes that are not open...

boxOpen = false; // set tag to show that box is closed again

window.clearTimeout(delay);

//var row = abc_num(fieldname);
//var col = parseInt(fieldname.substr(1));

// check if open cell should be recorded
//if ((statecont[RowOut[row]][ColOut[col]]=="0") & !(recOpenCells)) {return;}

thisElem = new objElem;
thisElem.name = fieldname;

// save procesdata
RecordEventData(thisElem, objEvent)

if (document.getElementById)  
	{
	// IE6/NS6>/Mozilla
	HandleTxt = document.getElementById(fieldname+"_td");
	HandleBox = document.getElementById(fieldname+"_box");
	}
	else if (document.all)
	{
	//IE4/5
	HandleTxt=eval("document.all['"+fieldname+"_td"+"']");
	HandleBox=eval("document.all['"+fieldname+"_box"+"']");
	}

HandleTxt.style.visibility='hidden';HandleBox.style.visibility='visible';
}

function recChoice(eventname ,name, value)
{
chkchoice = true;
timefunction(eventname, name, value);

if (document.forms[mlweb_fname].choice) {document.forms[mlweb_fname].choice.value = name;}

if (btnType=="button")
	{
	for (i=0;i<btnTxt.length;i++)
		{
		if (document.getElementById)  
			{
			// IE6/NS6>/Mozilla
			HandleTD = document.getElementById("btn_"+i.toString());
			}
			else if (document.all)
			{
			//IE4/5
			HandleTD=eval("document.all['"+"btn_"+i.toString()+"']");
			}
		HandleTD.style.backgroundColor = defTDcolor;
		if (btnFlg==1) 
			{
			if (btnTag[ColOut[i]]==name) {HandleTD.style.backgroundColor = btnColor;}
			}
			else
			{
			if (btnTag[RowOut[i]]==name) {HandleTD.style.backgroundColor = btnColor;}
			}
		}
	}
	
}
function loadMatrices()
{
// get settings data from script in body
txtcont = ExpMatrix(txt);
statecont = ExpMatrix(state);  
tagcont = ExpMatrix(tag);	
boxcont = ExpMatrix(box);
WidthCol = ExpRow(W_Col);
HeightRow = ExpRow(H_Row);
DTimes = ExpMatrix(delay);

CountCol = ExpRow(CBCol);
CountRow = ExpRow(CBRow);

btnTxt = ExpRow(btntxt);
btnTag = ExpRow(btntag);
btnState = ExpRow(btnstate);

ColOut = new Array();
for (var i=0; i<CountCol.length; i++)
{ColOut[i]=i;}

RowOut = new Array();
for (var i=0; i<CountRow.length; i++)
{RowOut[i]=i;}

Dlist = new Array();
for (j=0;j<RowOut.length;j++)
	{
	for (i=0;i<ColOut.length;i++)
		{
		if (statecont[j][i]=="1") {Dlist[Dlist.length]=tagcont[j][i];}
		}
	}
}

function reorder()
{
// if referer present (or other php/asp code) then get current hit number
if (document.cookie.indexOf("mlweb_subject=")!=-1)
		{
		subjstr=document.cookie;
		subject=subjstr.substr(subjstr.indexOf("mlweb_subject=")+14);
		}

if (document.cookie.indexOf("mlweb_condnum=")!=-1)
		{
		subjstr=document.cookie;
		subjnr=parseInt(subjstr.substr(subjstr.indexOf("mlweb_condnum=")+14));
		subjtype = "cookie";
		//alert(subjnr + " " + subjtype);
		}
		else 
		{
			if (typeof ref_cur_hit!="undefined")
				{subjnr = ref_cur_hit; subjtype = "header"}
				else
				{ // else get phpESP rid or randomize
					if (document.forms[mlweb_fname].rid) 
					{subjnr = parseInt(document.forms[0].rid.value);subjtype = "ESP_rid"}
					else 
					{subjnr=-1; subjtype = "random"};	
				}
		}

	// if subj nr turns out to be not a number, or randomizer is set to true then set it to randomize
	if (isNaN(subjnr)|randomOrder) {subjnr=-1; subjtype = "random";}

if (document.forms[mlweb_fname].condnum) {document.forms[mlweb_fname].condnum.value = subjnr;}
if (document.forms[mlweb_fname].expname) {document.forms[mlweb_fname].expname.value = expname;}
if (document.forms[mlweb_fname].nextURL) {document.forms[mlweb_fname].nextURL.value = nextURL;}
if (document.forms[mlweb_fname].subject) {document.forms[mlweb_fname].subject.value = subject;}
if (document.forms[mlweb_fname].to_email) {document.forms[mlweb_fname].to_email.value = to_email;}



// retrieve position of counterbalance groups 

var cf=new Array()  // position of fixed cols
var c1=new Array()  // position of c1 cols
var c2=new Array()  // position of c2 cols

for (var i=0; i<CountCol.length; i++)
	{
	switch (CountCol[i])
		{ 
		case '0': cf[cf.length]=i;break;
		case '1': c1[c1.length]=i;break;
		case '2': c2[c2.length]=i;break;
		}
	}

var rf=new Array()  // position of fixed rows
var r1=new Array()  // position of c1 rows
var r2=new Array()  // position of c2 rows

for (var i=0; i<CountRow.length; i++)
	{
	switch (CountRow[i])
		{ 
		case '0': rf[rf.length]=i;break;
		case '1': r1[r1.length]=i;break;
		case '2': r2[r2.length]=i;break;
		}
	}

// subjDen is the denominator used to devide the subj number for each counterbalance step

subjDen = 1;   

if (subjtype!="random") {subjDen = Math.floor(subjDen * masterCond)};
// first determine column and row connects and switch on that

// check consistency of group 1 and 2 of columns 
for (var i=0;i<c1.length;i++)
		{ //check if all c1's are before all c2s
		if (c1[i]>c2[0]) {connectCol = 0;break;}
		}
// check consistency of group 1 and 2 of rows
for (var i=0;i<r1.length;i++)
		{ //check if all c1's are before all c2s
		if (r1[i]>r2[0]) {connectRow = 0;break;}
		}

if ((c1.length==0) | (c2.length==0)) {connectCol = 0;}
if ((r1.length==0) | (r2.length==0)) {connectRow = 0;}

var numCond = (c1.length>0 ? fac(c1.length) : 1)*(c2.length>0 ? fac(c2.length) : 1)*(r1.length>0 ? fac(r1.length) : 1)*(r2.length>0 ? fac(r2.length) : 1)*(connectRow==1 ? 2 : 1)*(connectCol==1 ? 2 : 1);

if (subjnr==-1) {subjnr=Math.floor(Math.random()*numCond)}
//alert("total cond:" + numCond+"\nsubject: "+subjnr);

flipCols=false;   // initial state of col and row connects
flipRows=false;

// counterbalance col and row connects
if (connectCol==1) {
		if (Math.floor((subjnr/subjDen)/2+.5)>(subjnr/subjDen)/2) {flipCols = true} 
		subjDen = subjDen*2;
		}
if (connectRow==1) {
		if (Math.floor((subjnr/subjDen)/2+.5)>(subjnr/subjDen)/2) {flipRows = true} 
		subjDen = subjDen*2;
		}

// counterbalance col groups		
if (c1.length>0) {c1_order=CountBal(subjnr/subjDen+1,c1.length); 
					subjDen = subjDen*c1.length;} 
if (c2.length>0) {c2_order=CountBal(subjnr/subjDen+1,c2.length);
					subjDen = subjDen * c2.length;} 

var c1count=0, c2count=0;
ColOut = new Array();

for (var i=0; i<CountCol.length; i++)
	{
	switch (CountCol[i])
		{ 
		case '0': ColOut[i]=i;break;
		case '1': ColOut[i]=c1[c1_order[c1count]];c1count++;break;
		case '2': ColOut[i]=c2[c2_order[c2count]];c2count++;break;
		}
	}

// flips col groups 1 and 2 if Flipcols is true	
if (flipCols)
				{// flip col group 1 and 2	
				tempColOut = new Array()
				tempColOut = ColOut.join().split(",");
				//alert(tempColOut[1]);
				c1count=0;c2count=0;
				for (var i=0; i<CountCol.length; i++)
					{
					if (CountCol[i]!="0") 
								{if (c2count<c2.length) 		 
									{ColOut[i]=tempColOut[c2[c2count]];c2count++}
									else
								{ColOut[i]=tempColOut[c1[c1count]];c1count++}
								}
					}
				}

// counterbalance rows					
if (r1.length>0) {r1_order=CountBal(subjnr/subjDen+1,r1.length); subjDen = subjDen * r1.length;} 

if (r2.length>0) {r2_order=CountBal(subjnr/subjDen+1,r2.length);
subjDen = subjDen * r2.length;}

var r1count=0, r2count=0;
RowOut = new Array();

for (var i=0; i<CountRow.length; i++)
	{
	switch (CountRow[i])
		{ 
		case '0': RowOut[i]=i;break;
		case '1': RowOut[i]=r1[r1_order[r1count]];r1count++;break;
		case '2': RowOut[i]=r2[r2_order[r2count]];r2count++;break;
		}
	}

// flip row groups if flipRows is true
if (flipRows)
				{// flip col group 1 and 2	
				tempRowOut = new Array()
				tempRowOut = RowOut.join().split(",");
				//alert(tempRowOut.join());
				r1count=0;r2count=0;
				for (var i=0; i<CountRow.length; i++)
					{
					if (CountRow[i]!="0") 
								{if (r2count<r2.length) 		 
									{RowOut[i]=tempRowOut[r2[r2count]];r2count++}
									else
								{RowOut[i]=tempRowOut[r1[r1count]];r1count++}
								}
					}
				}

Dlist=new Array();

// reorder and resize table content
				
for (j=0;j<RowOut.length;j++)
	{
	for (i=0;i<ColOut.length;i++)
		{
		var label = String.fromCharCode(j+97)+i.toString();
		if (statecont[j][i]=="1") {Dlist[Dlist.length]=tagcont[j][i];}

		if (document.getElementById)  
			{
			// IE6/NS6>/Mozilla
			HandleCont = document.getElementById(label+"_cont");
			HandleTxt = document.getElementById(label+"_txt");
			HandleBox = document.getElementById(label+"_box");
			HandleTD = document.getElementById(label+"_td");
			HandleTDbox = document.getElementById(label+"_tdbox");
			pxstr="px"
			}
			else if (document.all)
			{
			//IE4/5
			HandleCont=eval("document.all['"+label+"_cont"+"']");
			HandleTxt=eval("document.all['"+label+"_txt"+"']");
			HandleTD=eval("document.all['"+label+"_td"+"']");
			HandleBox=eval("document.all['"+label+"_box"+"']");
			HandleTDbox=eval("document.all['"+label+"_tdbox"+"']");
			pxstr="px";
			}
		
		// set txt 
		HandleTD.innerHTML =""; // empty for IE5 on mac bug
		// if txtcont is empty or only contains spaces then replace by nbsp to keep TD layout
		if (txtcont[RowOut[j]][ColOut[i]].replace(/[\x20]/gi, "")=="") {HandleTD.innerHTML = "&nbsp;"} else {HandleTD.innerHTML = txtcont[RowOut[j]][ColOut[i]]};

		HandleTDbox.innerHTML =""; // empty for IE5 on mac bug
		// if boxcont is empty or only contains spaces then replace by nbsp to keep TD layout
		if (boxcont[RowOut[j]][ColOut[i]].replace(/[\x20]/gi, "")=="") {HandleTDbox.innerHTML = "&nbsp;"} else {HandleTDbox.innerHTML = boxcont[RowOut[j]][ColOut[i]]};
		
		//set sizes
		HandleTD.width = parseInt(WidthCol[ColOut[i]])-5;
		HandleTD.height = parseInt(HeightRow[RowOut[j]])-5;
		HandleTDbox.width = parseInt(WidthCol[ColOut[i]])-5;
		HandleTDbox.height = parseInt(HeightRow[RowOut[j]])-5;
		if (statecont[RowOut[j]][ColOut[i]]=="1") {HandleTD.className = activeClass;} else {HandleTD.className = inactiveClass};
		HandleCont.style.width = parseInt(WidthCol[ColOut[i]])+pxstr;
		HandleCont.style.height = parseInt(HeightRow[RowOut[j]])+pxstr;
		HandleTxt.style.width = parseInt(WidthCol[ColOut[i]])+pxstr;
		HandleTxt.style.height = parseInt(HeightRow[RowOut[j]])+pxstr;
		HandleTxt.style.clip = "rect(0px "+ HandleTxt.style.width + " " + HandleTxt.style.height +" 0px)";
		HandleBox.style.width = parseInt(WidthCol[ColOut[i]])+pxstr;
		HandleBox.style.height = parseInt(HeightRow[RowOut[j]])+pxstr;
		HandleBox.style.clip = "rect(0px "+ HandleBox.style.width + " " + HandleBox.style.height +" 0px)";
		
		// open state=0 boxes using img names from imgcont matrix
		if (statecont[RowOut[j]][ColOut[i]] == '0')	{HandleBox.style.visibility = "hidden"; HandleTxt.style.visibility = "visible";} else {HandleBox.style.visibility = "visible"; HandleTxt.style.visibility = "hidden";}

		}
	}
	// if there are buttons then reorder the buttons according to the counterbalancing scheme
	if (btnFlg>0)
		{
		for (bc=0;bc<btnTxt.length;bc++)
			{				
			if (document.getElementById)  
				{
					// IE6/NS6>/Mozilla
				HandleTD = document.getElementById("btn_"+bc.toString());
				}
				else if (document.all)
				{
				//IE4/5
				HandleTD=eval("document.all['"+"btn_"+bc.toString()+"']");
				}
			if (bc==0) {defTDcolor = HandleTD.style.backgroundColor;}
			if (btnFlg==1) {var btnNum = parseInt(ColOut[bc])} else {var btnNum = parseInt(RowOut[bc])};
			docstr="";
			if (btnState[btnNum]=="1") 
						{
						var functionstr = "onMouseOver=\"timefunction('mouseover','"+btnTag[btnNum]+"','"+btnTxt[btnNum]+"')\" onClick=\"recChoice('onclick','"+btnTag[btnNum]+"','"+btnTxt[btnNum]+"')\" onMouseOut=\"timefunction('mouseout','"+btnTag[btnNum]+"','"+btnTxt[btnNum]+"')\""; 
						if (btnType=="radio") {docstr+="<INPUT type=\"radio\" name=\"mlchoice\" value=\""+btnTag[btnNum]+"\" "+functionstr+">"+btnTxt[btnNum]} 
						   			    else {docstr+="<INPUT type=\"button\" name=\"" + btnTag[btnNum] + "\" value=\""+btnTxt[btnNum]+"\" "+functionstr+">"}; 
						}
						else
						{docstr+="&nbsp;";}

			HandleTD.innerHTML =""; // empty for IE5 on mac bug
			HandleTD.innerHTML = docstr;
			}
		}

// send col and row orders as events
timefunction("subject", subjtype,subjnr)
timefunction("order","col",ColOut.join("_"))
timefunction("order","row",RowOut.join("_"))
if (typeof serial!="undefined") {timefunction("ref_vars","serial", serial)}
loaded = true; // set flag that page has been loaded;
	return;
}

/////////////////////////
// end of mouselabcode //
/////////////////////////