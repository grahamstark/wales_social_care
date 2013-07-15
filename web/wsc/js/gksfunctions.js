/****************************************************
     Author: Eric King
     Url: http://redrival.com/eak/index.shtml
     This script is free to use as long as this info is left in
     Featured on Dynamic Drive script library (http://www.dynamicdrive.com)
****************************************************/
     
/* some handly Javascript functions, collected (rather than written...) by Graham Stark
 * build a pop-up window and give it focus.
 **/

 function DataWindow(  mypage, title ){
       settings='width=1000,height=900,top=40,left=30,scrollbars=yes,location=yes,directories=no,status=no,menubar=yes,toolbar=no,resizable=yes';
       w = window.open(mypage, title, settings );
       w.focus();
 }
 
function ExampleWindow( mypage, rowTitle, colTitle ){
       LeftPosition=(screen.width)?(screen.width-950)/2:100;
       TopPosition=(screen.height)?(screen.height-450)/2:100;
       settings='width=1200,height=800,top='+TopPosition+',left='+LeftPosition+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open( mypage, 'Example', settings );
       w.focus();
}

function HelpWindow( mypage ){
       LeftPosition=(screen.width)?(screen.width-950)/2:100;
       TopPosition=(screen.height)?(screen.height-450)/2:100;
       settings='width=400,height=400,top='+TopPosition+',left='+LeftPosition+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open(mypage, 'help' ,settings);
       w.focus();
}

function TutorialWindow( mypage ){
       LeftPosition=(screen.width)?(screen.width-950)/2:100;
       TopPosition=(screen.height)?(screen.height-450)/2:100;
       settings='width=1000,height=900,top='+TopPosition+',left='+LeftPosition+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open(mypage, 'help' ,settings);
       w.focus();
}


function TimeSeriesWindow( mypage ){
       LeftPosition=(screen.width)?(screen.width-950)/2:100;
       TopPosition=(screen.height)?(screen.height-450)/2:100;
       settings='width=800,height=330,top='+TopPosition+',left='+LeftPosition+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open(mypage, 'help' ,settings);
       w.focus();
}


function PictureWindow( mypage, width, height ){
       //LeftPosition=(screen.width)?(screen.width-950)/2:100;
       //TopPosition=(screen.height)?(screen.height-450)/2:100;
       // FIXME: just right of where we were clicked
       settings='top=60,left=220,width='+width+',height='+height+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open(mypage, 'help' ,settings);
       w.focus();
}

function GalleryWindow( mypage, title1, width, height){
       settings='top=60,left=220,width='+width+',height='+height+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open( mypage, title1 ,settings);
       w.focus();
}

function ForumWindow( mypage ){
       LeftPosition=(screen.width)?(screen.width-950)/2:100;
       TopPosition=(screen.height)?(screen.height-450)/2:100;
       settings='width=700,height=800,top='+TopPosition+',left='+LeftPosition+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
       w = window.open(mypage, 'help' ,settings);
       w.focus();     
}

function trimString(sInString) {
  sInString = sInString.replace( /^\s+/g, "" );// strip leading
  return sInString.replace( /\s+$/g, "" );// strip trailing
}

/** 
 * borrowed from http://www.breakingpar.com/bkp/home.nsf/0/87256B280015193F87256C720080D723
 */
function isArray(obj) {
   if (obj.constructor.toString().indexOf("Array") == -1)
      return false;
   else
      return true;
}

/**
 *  Call the overlib with the given help text if 
 *  there is a checkbox with Id 'HoverhelpOn' and the value is 1 or true
 *  or if the field doesn't exist and the help text is not null or blank
*/
function overlibConditional( helpStr ){
       var showHelp = 1;
       if( helpStr == null ){
                return 0;       
       }
       var helpStr = trimString(helpStr);
       if( helpStr.length == 0 ){
                return 0;
       }
       mycheckBox = document.getElementById('HoverhelpOn');
       if( mycheckBox != null){
                showHelp = ((mycheckBox.value == 'true' ) || (mycheckBox.value == '1' ));
       } 
       if( showHelp ){
               return overlib( helpStr );
       }
}

function rookerWise( v, changeAmount, next ){
        if( changeAmount == 0 ){
                iadd = 0.0;       
        } else {
                iadd = Math.round((v*(changeAmount)/next));
                add = (iadd*next) + next;
        }
        return v+add;       
}

/**
 * Set data in the input fields for the given variable, and clear any error fields
 * (since we wouldn't be setting invalid data this way, would we?).
*/
function setData( which, startYear, endYear, data, dataType, prec ){
        p = 0;
        for( year = startYear; year <= endYear; year++ ){
                target = which+"_"+year;
                thisYearsInput = $( target );
                thisYearsInput.value = format(data[p], dataType, prec );
                // Clear any associated error fields. These are right beside
                // the input box, so we can just delete any siblings of the input,
                // and clear the 'inputerror' class
                input_class = thisYearsInput.getAttribute( 'class' ) 
                if( input_class == 'inputerror' ){
                        sibs = thisYearsInput.siblings();
                        for( i = 0; i < sibs.length; i++ ){
                                sibs[i].remove();       
                        }
                        thisYearsInput.removeAttribute( 'class' )
                }
                p++;
        }
}

/**
 * String to int/float/string depending on the value of type: 
 * FIXME we need real|integer|enumerated|boolean|string|date
*/
function parse( str, type ){
        // 
        switch( type ){
        case 'real' :
                return parseFloat( str );
        case 'integer' :
                return parseInt( str );
        case 'boolean' :
                return ( str != null )? 'on' : null; // UNTESTED: assumes checkbox.
        default :
                return str;
        }
}

function setChangedClass( targetElem, key ){
        now = $( key ).innerHTML;
        then = $( 'base_' + key ).value;
        // alert( "key "+key + " now " + now + " then " + then );
        if( parseFloat( now ) != parseFloat( then )){
                targetElem.setAttribute( 'class', 'changed' );
        }
}


function format( v, dataType, prec ){
        // FIXME we need real|integer|enumerated|boolean|string|date
        switch( dataType ){
        case 'real' :
                return v.toFixed( prec );
        case 'integer' :
                return Math.round(v);
        default :
              return v;
        }
}

function allElementsForYear( keys, year ){
        foryear = new Array();
        for( i = 0; i < keys.length; i++ ){
                key = keys[i]+'_'+year;
                foryear[i] = parse( $( key ).value, 'real' );
        }
        return foryear;
}

function sumArray( arr ){
        sum = 0.0;
        for( i = 0; i < arr.length-1; i++ ){
                sum += arr[i];
        }
        return sum;
}

function setResidual( arr ){
        residual = 100.0 - sumArray( arr );
        // alert( "residual is " + residual );
        arr[ arr.length-1 ] = residual;
        return arr;
}

/**
* For the multi-field popup handlers: check if all the variables for the given year with ids in the set keys
* sum to 100.0 and print an error message if not on the error div of the bottommost item.
* @param keys - array of strings (ids) of the elements whose values we sum over
* @param year - 2006, etc.
* @return 1 if we sum to 100, -1 otherwise;
*/
function validateSum( keys, year ){
        vals = allElementsForYear( keys, year );
        sum = sumArray( vals );
        endkey = keys[ keys.length - 1 ]
        residualElementId = endkey+"_"+year;
        // alert( "numKeys = " + keys.length +  " residualElementId  = " + residualElementId );
        errorId = residualElementId + "_error";
        err = 1;
        if( sum > 100.0 ){
                messageDiv = document.getElementById( errorId );
                messageDiv.innerHTML = "Error: your inputs for this year sum to more than 100.";
                messageDiv.setAttribute( 'class', 'error' );
                err = -1;
        } else {
                vals = setResidual( vals );
                // alert( "made vals as " + vals );
                messageDiv = document.getElementById( residualElementId );
                messageDiv.innerHTML = vals[ vals.length - 1 ];//format( vals[ keys.length - 1 ], 'real' , 2 );
                setChangedClass( messageDiv, endkey+"_"+year );
                messageDiv = document.getElementById( errorId );
                messageDiv.innerHTML = '';
                messageDiv.setAttribute( 'class', 'hidden_element' );
                
        }
        return err;
}




/**
 * Checks that the element at currentKey is a valid number between min and max, and
 * and if sumsToHundred is true, cheks that all the elements in keys array for the given
 * year sums to 100, and adds an error message on the bottom item if not.
 * FIXME changed class not set properly on the input field (but is on the sum field). 
*/
function validateMultiInput( currentKey, year, keys, min, max, sumsToHundred ){
        key = currentKey+"_"+year;
        vstr = $( key ).value;
        numberRegex = / *^[-+]?[0-9]*\.?[0-9]+$ */;
        messageDiv = $( key+"_error" );
        errmsg = '';
        // alert( "keys.length " + keys.length );
        if( numberRegex.test( vstr )){
                v = parse( vstr, 'real' );
                if( v > max ){
                        errmsg = "This needs to be no more than "+max;
                } else if( v < min ){
                        errmsg = "This needs to be at least "+min;
                }
        } else {
                errmsg = "This needs to be a valid number";       
        }
        if( errmsg == '' ){
                messageDiv.innerHTML = '';
                messageDiv.setAttribute( 'class', 'hidden_element' );
                if( sumsToHundred == 1 ){
                        if( validateSum( keys, year ) == 1 ){
                                setChangedClass( $( key ), key );
                        };
                }
        } else {
                messageDiv.innerHTML = errmsg;
                messageDiv.setAttribute( 'class', 'error' );
        }
}

/**
 * FIXME: massive duplication because of quick hacks to work with multi-period  
 * 
*/
function multi_operateOnPeriods( keys, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec ){
                
                for( i = 0; i < keys.length; i++ ){
                        which = keys[i];
                        startYearsInput = document.getElementById( which+"_"+startYear );                        
                        for( year = startYear; year <= endYear; year++ ){
                               
                                thisYearsInput = $( which+"_"+year );
                                v = parse(startYearsInput.value, dataType );
                                valStr = thisYearsInput.value;
                                // error checking goes here
                                switch( op ){
                                case 'constant':
                                        v = parse( valStr, dataType );
                                        v = changeAmount;
                                        break                        
                                case 'add' :
                                        v = parse( valStr, dataType );
                                        v += changeAmount;
                                        break
                                case 'minus' :
                                        v = parse( valStr, dataType );
                                        v -= changeAmount;
                                        break;
                                case 'times' :
                                        v = parse( valStr, dataType );
                                        if( uprateType == 'rooker_wise' ){
                                                v = rookerWise( v, changeAmount, next );       
                                        } else {
                                                v *= changeAmount;
                                        }
                                        break;
                                case 'grow' :       
                                        if( year > startYear ){
                                                if( uprateType == 'rooker_wise' ){
                                                        v = rookerWise( v, changeAmount-1.0, next );       
                                                } else {
                                                        v *= changeAmount;
                                                }  
                                        }
                                }
                                thisYearsInput.value = format( v, dataType, prec );
                        }
                }
}



/**
 * FIXME: For FLOATING POINT types ONLY !!
 * 
*/
function single_operateOnPeriods( which, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec ){
                startYearsInput = document.getElementById( which+"_"+startYear );
                v = parse(startYearsInput.value, dataType );
                for( year = startYear; year <= endYear; year++ ){
                        thisYearsInput = $( which+"_"+year );
                        valStr = thisYearsInput.value;
                        // error checking goes here
                        switch( op ){
                        case 'constant':
                                v = parse( valStr, dataType );
                                v = changeAmount;
                                break                        
                        case 'add' :
                                v = parse( valStr, dataType );
                                v += changeAmount;
                                break
                        case 'minus' :
                                v = parse( valStr, dataType );
                                v -= changeAmount;
                                break;
                        case 'times' :
                                v = parse( valStr, dataType );
                                if( uprateType == 'rooker_wise' ){
                                        v = rookerWise( v, changeAmount, next );       
                                } else {
                                        v *= changeAmount;
                                }
                                break;
                        case 'grow' :       
                                if( year > startYear ){
                                        if( uprateType == 'rooker_wise' ){
                                                v = rookerWise( v, changeAmount-1.0, next );       
                                        } else {
                                                v *= changeAmount;
                                        }  
                                }
                        }
                        thisYearsInput.value = format( v, dataType, prec );    
                }
}

/**
* FIXME: mess: duplicated !! 
 * Check if which is an array (multi-variable case) and dispach 
*/
function operateOnPeriods( which, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec ){
        if( isArray( which )){
                multi_operateOnPeriods( which, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec );       
        } else {
                single_operateOnPeriods( which, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec );
        }
}

function multiply_exact( which, startYear, endYear, errorMessage ){
        select = $('select_options');
        selectValue = select.value;
        var temp = new Array();
        temp = selectValue.split( "_" );
        var op = temp[0];
        var val = temp[1];
       // alert( "got select value as "+selectValue+" op="+op+" val = "+val );
        if( op != 'nochange' ){
                changeAmount  = parseFloat( val );
                operateOnPeriods( which, startYear, endYear, op, changeAmount );         
        }
        // always put back
        select.value = 'nochange_0.0';
}

/**
 * Perform a javascript mass-update of one of our model input arrays. 
 * The routine looks for input elements with an id of the form <some identifier>_<some_year>
 * extracts the fields text, converts it to a number, operates on the number and writes it back.
 * If the number to add is invalid we look for a div of the form uprate_amount_error_<some identifier> and
 * write an error message into it using the DOM.
 * 
 * targetElements - key identifier used in each element of this field block. Either array of single item
 * startYear - e.g. 2005
 * endYear - e.g. 2010
*/
function uprateBlock( targetElements, startYear, endYear, errorMessage, uprateType, next, dataType, prec ){
        numberRegex = / *^[-+]?[0-9]*\.?[0-9]+$ */;
        if( ! isArray( targetElements )){
                toLoopOver = new Array();
                toLoopOver[0] = targetElements;
        } else {
                toLoopOver = targetElements;
        }
        for( k = 0; k < toLoopOver.length; k++ ){
                which = toLoopOver[k];
                uprateElement = document.getElementById( 'uprate_amount_v' );
                changeStr = uprateElement.value;
                messageDiv = $( 'uprate_amount_error_v' );
                if( numberRegex.test( changeStr )){
                        changeAmount = parse( changeStr, dataType );
                        // any error message gets blanked
                        messageDiv.innerHTML = '';
                        messageDiv.setAttribute( 'class', 'hidden_element' );
                        op = 'constant'; 
                        if( $( 'add_v' ).checked ){
                                op = 'add';
                        } else if( (dataType == 'real') && 
                                   ( $( 'upby_growth_v' ) != null ) && 
                                   ( $( 'upby_growth_v' ).checked )){
                                op = 'grow';
                                changeAmount = 1.0 + ( changeAmount/100.0 );
                        }
                        operateOnPeriods( which, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec );
                 } else {
                        messageDiv.innerHTML = errorMessage;
                        messageDiv.setAttribute( 'class', 'error' );
                 }
        }
}

function changeIntegerBlock( which, startYear, endYear, errorMessage ){
        uprateElement = document.getElementById( 'uprate_amount_v' );
        numberRegex = / *^[-+]?[0-9]+$ */;
        changeStr = uprateElement.value;
        messageDiv = document.getElementById( 'uprate_amount_error_v' );
        if( numberRegex.test( changeStr )){
                changeAmount = parseInt( changeStr );
                messageDiv.innerHTML = '';
                messageDiv.setAttribute( 'class', 'hidden_element' );
                for( year = startYear; year <= endYear; year++ ){
                        thisYearsInput = $( which+"_"+year );
                        valStr = thisYearsInput.value;
                        v = parseInt( valStr );
                        v += changeAmount;
                        thisYearsInput.value = v.toFixed(0);    
                }
         } else {
                messageDiv.innerHTML = errorMessage;
                messageDiv.setAttribute( 'class', 'error' );
         }
}

/**
 * which, 
 * startYear, 
 * endYear, 
 * errorMessage, 
 * uprateType, 
 * next
*/
function operateOnMenu( which, startYear, endYear, errorMessage, uprateType, next, dataType, prec ){
        menu = $('select_options');
        selectedIndex = menu.options.selectedIndex;
        selectedItem =  menu.options[selectedIndex].value;      
        sp = selectedItem.split( "_" );
        op = sp[0];
        changeAmount = parse( sp[1], dataType );
        // alert( "got selected item as op" + op + " changeAmount " + changeAmount );
        if( ( op == 'grow' ) && ( dataType == 'real' )){
                changeAmount = 1.0 + ( changeAmount/100.0 );       
        }
        operateOnPeriods( which, startYear, endYear, op, changeAmount, uprateType, next, dataType, prec );
        /** selected items can be :: grow, add, times, no change constant **/
        /*|dont_uprate, rooker_wise|standard_uprate */
}

/**
 * Drive a 'jump to' select box
*/
function jumpTo( whichSelect ){
        sel = $( whichSelect );
        index = sel.selectedIndex;
        destination = sel.options[index].value;
        if( destination != null ){
                location.href = destination;
        }
}

/**
 * Simple driver for a flash demonstration in the 'helppage.php' window popup. This:
 * - plays the file <movie>.swf in a region 'flashcontent'
 * - if movie is blank inserts a picture /tutorials/images/flash_holder.png
 * @param movie - name of flash file, minus the 'swf' extension
 * @param h1text - string for the title. 
*/

function playFlash( movie, h1text ){ 
        if (( movie == '' ) || ( movie == null )){
               dummy = document.createElement( 'img' );
               dummy.setAttribute( 'src', '/tutorials/images/flash_holder.png' ); 
               $( 'flashcontent' ).appendChild(dummy);       
        } else {
                if( movie == 'output_tour_project' ){
                        so = new SWFObject( movie+".swf", "sotester", 640, 480, "9", "#FFFFFF");
                } else {
                        so = new SWFObject( movie+".swf", "sotester", 1000, 800, "9", "#FFFFFF");
                }
                so.addVariable("play", true );
                so.addVariable("loop", false );
                so.addVariable("quality", "high" );
                so.write("flashcontent");
                $( 'toptitle' ).innerHTML = h1text;
                lis = $$( '#helplist li a' );
                for( i = 0; i < lis.length; i++ ){
                        id = lis[i].getAttribute( 'id' );
                        if( id ==  'menu_'+movie ){
                                lis[i].setAttribute( 'class', 'selected' );  
                        } else {
                                lis[i].setAttribute( 'class', 'normal' );
                        }
                }
 
        }
}

function selectOption( target, value ){
        day_options = $( target ).getElementsByTagName( 'option' );
        alert( day_options );
        for( i = 0; i < day_options.length; i++ ){
                //alert( "option["+i+"] = "+day_options[i] );
                if( parseInt(day_options[i].value) == value ){
                        alert( "found " + i );
                        day_options[i].setAttribute( 'selected', 'selected' );
                        break;
                }
        }
}

function selectDate( id, xdate ){
        year = xdate.getFullYear();
        month = xdate.getMonth()+1;
        day = xdate.getDate();
        selectOption( id+'_day', day );
        selectOption( id+'_month', month );
        selectOption( id+'_year', year );
}

function setDefaultDates(){
        today = new Date();
        tomorrow = new Date();
        tomorrow.setDate(tomorrow.getDate()+1);
        //alert( today );
        //alert( tomorrow );
        selectDate( 'start', today );
        selectDate( 'end', tomorrow );
}        

