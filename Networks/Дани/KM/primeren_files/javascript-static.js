M.yui.add_module=function(modules){for(var modname in modules)YUI_config.modules[modname]=modules[modname]};M.yui.galleryversion='2010.04.21-21-51';M.util=M.util||{};M.str=M.str||{};M.util.image_url=function(imagename,component){if(!component||component==''||component=='moodle'||component=='core')component='core';var url=M.cfg.wwwroot+'/theme/image.php';if(M.cfg.themerev>0&&M.cfg.slasharguments==1){if(!M.cfg.svgicons)url+='/_s';url+='/'+M.cfg.theme+'/'+component+'/'+M.cfg.themerev+'/'+imagename}else{url+='?theme='+M.cfg.theme+'&component='+component+'&rev='+M.cfg.themerev+'&image='+imagename;if(!M.cfg.svgicons)url+='&svg=0'};return url};M.util.in_array=function(item,array){for(var i=0;i<array.length;i++)if(item==array[i])return true;return false};M.util.init_collapsible_region=function(Y,id,userpref,strtooltip){Y.use('anim',function(Y){new M.util.CollapsibleRegion(Y,id,userpref,strtooltip)})};M.util.CollapsibleRegion=function(Y,id,userpref,strtooltip){this.userpref=userpref;this.div=Y.one('#'+id);var caption=this.div.one('#'+id+'_caption'),a=Y.Node.create('<a href="#"></a>');a.setAttribute('title',strtooltip);while(caption.hasChildNodes()){child=caption.get('firstChild');child.remove();a.append(child)};caption.append(a);var height=this.div.get('offsetHeight'),collapsedimage='t/collapsed';if(right_to_left()){collapsedimage='t/collapsed_rtl'}else collapsedimage='t/collapsed';if(this.div.hasClass('collapsed')){this.icon=Y.Node.create('<img src="'+M.util.image_url(collapsedimage,'moodle')+'" alt="" />');this.div.setStyle('height',caption.get('offsetHeight')+'px')}else this.icon=Y.Node.create('<img src="'+M.util.image_url('t/expanded','moodle')+'" alt="" />');a.append(this.icon);var animation=new Y.Anim({node:this.div,duration:0.3,easing:Y.Easing.easeBoth,to:{height:caption.get('offsetHeight')},from:{height:height}});animation.on('end',function(){this.div.toggleClass('collapsed');var collapsedimage='t/collapsed';if(right_to_left()){collapsedimage='t/collapsed_rtl'}else collapsedimage='t/collapsed';if(this.div.hasClass('collapsed')){this.icon.set('src',M.util.image_url(collapsedimage,'moodle'))}else this.icon.set('src',M.util.image_url('t/expanded','moodle'))},this);a.on('click',function(e,animation){e.preventDefault();if(animation.get('running'))animation.stop();animation.set('reverse',this.div.hasClass('collapsed'));if(this.userpref)M.util.set_user_preference(this.userpref,!this.div.hasClass('collapsed'));animation.run()},this,animation)};M.util.CollapsibleRegion.prototype.userpref=null;M.util.CollapsibleRegion.prototype.div=null;M.util.CollapsibleRegion.prototype.icon=null;M.util.set_user_preference=function(name,value){YUI().use('io',function(Y){var url=M.cfg.wwwroot+'/lib/ajax/setuserpref.php?sesskey='+M.cfg.sesskey+'&pref='+encodeURI(name)+'&value='+encodeURI(value),cfg={method:'get',on:{}};if(M.cfg.developerdebug)cfg.on.failure=function(id,o,args){alert("Error updating user preference '"+name+"' using ajax. Clicking this link will repeat the Ajax call that failed so you can see the error: ")};Y.io(url,cfg)})};M.util.show_confirm_dialog=function(e,args){var target=e.target;if(e.preventDefault)e.preventDefault();YUI().use('moodle-core-notification-confirm',function(Y){var confirmationDialogue=new M.core.confirm({width:'300px',center:true,modal:true,visible:false,draggable:false,title:M.util.get_string('confirmation','admin'),noLabel:M.util.get_string('cancel','moodle'),question:args.message});confirmationDialogue.on('complete-yes',function(e){if(args.callback){if(!Y.Lang.isFunction(args.callback)){Y.log('Callbacks to show_confirm_dialog must now be functions. Please update your code to pass in a function instead.','warn','M.util.show_confirm_dialog');return};var scope=e.target;if(Y.Lang.isObject(args.scope))scope=args.scope;var callbackargs=args.callbackargs||[];args.callback.apply(scope,callbackargs);return};var targetancestor=null,targetform=null;if(target.test('a')){window.location=target.get('href')}else if((targetancestor=target.ancestor('a'))!==null){window.location=targetancestor.get('href')}else if(target.test('input')){targetform=target.ancestor('form',true);if(!targetform)return;if(target.get('name')&&target.get('value'))targetform.append('<input type="hidden" name="'+target.get('name')+'" value="'+target.get('value')+'">');targetform.submit()}else if(target.test('form')){target.submit()}else Y.log("Element of type "+target.get('tagName')+" is not supported by the M.util.show_confirm_dialog function. Use A, INPUT, or FORM",'warn','javascript-static')},this);if(args.cancellabel)confirmationDialogue.set('noLabel',args.cancellabel);if(args.continuelabel)confirmationDialogue.set('yesLabel',args.continuelabel);confirmationDialogue.render().show()})};M.util.init_maximised_embed=function(Y,id){var obj=Y.one('#'+id);if(!obj)return;var get_htmlelement_size=function(el,prop){if(Y.Lang.isString(el))el=Y.one('#'+el);if(el){var val=el.getStyle(prop);if(val=='auto')val=el.getComputedStyle(prop);val=parseInt(val);if(isNaN(val))return 0;return val}else return 0},resize_object=function(){obj.setStyle('display','none');var newwidth=get_htmlelement_size('maincontent','width')-35;if(newwidth>500){obj.setStyle('width',newwidth+'px')}else obj.setStyle('width','500px');var headerheight=get_htmlelement_size('page-header','height'),footerheight=get_htmlelement_size('page-footer','height'),newheight=parseInt(Y.one('body').get('docHeight'))-footerheight-headerheight-100;if(newheight<400)newheight=400;obj.setStyle('height',newheight+'px');obj.setStyle('display','')};resize_object();Y.use('event-resize',function(Y){Y.on("windowresize",function(){resize_object()})})};M.util.init_frametop=function(Y){Y.all('a').each(function(node){node.set('target','_top')});Y.all('form').each(function(node){node.set('target','_top')})};M.util.init_toggle_class_on_click=function(Y,id,cssselector,toggleclassname,togglecssselector){if(togglecssselector=='')togglecssselector=cssselector;var node=Y.one('#'+id);node.all(cssselector).each(function(n){n.on('click',function(e){e.stopPropagation();if(e.target.test(cssselector)&&!e.target.test('a')&&!e.target.test('img'))if(this.test(togglecssselector)){this.toggleClass(toggleclassname)}else this.ancestor(togglecssselector).toggleClass(toggleclassname)},n)});node.on('click',function(e){if(e.target.hasClass('addtoall')){this.all(togglecssselector).addClass(toggleclassname)}else if(e.target.hasClass('removefromall'))this.all(togglecssselector+'.'+toggleclassname).removeClass(toggleclassname)},node)};M.util.init_colour_picker=function(Y,id,previewconf){Y.use('node','event-mouseenter',function(){var colourpicker={box:null,input:null,image:null,preview:null,current:null,eventClick:null,eventMouseEnter:null,eventMouseLeave:null,eventMouseMove:null,width:300,height:100,factor:5,init:function(){this.input=Y.one('#'+id);this.box=this.input.ancestor().one('.admin_colourpicker');this.image=Y.Node.create('<img alt="" class="colourdialogue" />');this.image.setAttribute('src',M.util.image_url('i/colourpicker','moodle'));this.preview=Y.Node.create('<div class="previewcolour"></div>');this.preview.setStyle('width',this.height/2).setStyle('height',this.height/2).setStyle('backgroundColor',this.input.get('value'));this.current=Y.Node.create('<div class="currentcolour"></div>');this.current.setStyle('width',this.height/2).setStyle('height',this.height/2-1).setStyle('backgroundColor',this.input.get('value'));this.box.setContent('').append(this.image).append(this.preview).append(this.current);if(typeof previewconf==='object'&&previewconf!==null)Y.one('#'+id+'_preview').on('click',function(e){if(Y.Lang.isString(previewconf.selector)){Y.all(previewconf.selector).setStyle(previewconf.style,this.input.get('value'))}else for(var i in previewconf.selector)Y.all(previewconf.selector[i]).setStyle(previewconf.style,this.input.get('value'))},this);this.eventClick=this.image.on('click',this.pickColour,this);this.eventMouseEnter=Y.on('mouseenter',this.startFollow,this.image,this)},startFollow:function(e){this.eventMouseEnter.detach();this.eventMouseLeave=Y.on('mouseleave',this.endFollow,this.image,this);this.eventMouseMove=this.image.on('mousemove',function(e){this.preview.setStyle('backgroundColor',this.determineColour(e))},this)},endFollow:function(e){this.eventMouseMove.detach();this.eventMouseLeave.detach();this.eventMouseEnter=Y.on('mouseenter',this.startFollow,this.image,this)},pickColour:function(e){var colour=this.determineColour(e);this.input.set('value',colour);this.current.setStyle('backgroundColor',colour)},determineColour:function(e){var eventx=Math.floor(e.pageX-e.target.getX()),eventy=Math.floor(e.pageY-e.target.getY()),imagewidth=this.width,imageheight=this.height,factor=this.factor,colour=[255,0,0],matrices=[[0,1,0],[-1,0,0],[0,0,1],[0,-1,0],[1,0,0],[0,0,-1]],matrixcount=matrices.length,limit=Math.round(imagewidth/matrixcount),heightbreak=Math.round(imageheight/2);for(var x=0;x<imagewidth;x++){var divisor=Math.floor(x/limit),matrix=matrices[divisor];colour[0]+=matrix[0]*factor;colour[1]+=matrix[1]*factor;colour[2]+=matrix[2]*factor;if(eventx==x)break};var pixel=[colour[0],colour[1],colour[2]];if(eventy<heightbreak){pixel[0]+=Math.floor(((255-pixel[0])/heightbreak)*(heightbreak-eventy));pixel[1]+=Math.floor(((255-pixel[1])/heightbreak)*(heightbreak-eventy));pixel[2]+=Math.floor(((255-pixel[2])/heightbreak)*(heightbreak-eventy))}else if(eventy>heightbreak){pixel[0]=Math.floor((imageheight-eventy)*(pixel[0]/heightbreak));pixel[1]=Math.floor((imageheight-eventy)*(pixel[1]/heightbreak));pixel[2]=Math.floor((imageheight-eventy)*(pixel[2]/heightbreak))};return this.convert_rgb_to_hex(pixel)},convert_rgb_to_hex:function(rgb){var hex='#',hexchars="0123456789ABCDEF";for(var i=0;i<3;i++){var number=Math.abs(rgb[i]);if(number==0||isNaN(number)){hex+='00'}else hex+=hexchars.charAt((number-number%16)/16)+hexchars.charAt(number%16)};return hex}};colourpicker.init()})};M.util.init_block_hider=function(Y,config){Y.use('base','node',function(Y){M.util.block_hider=M.util.block_hider||(function(){var blockhider=function(){blockhider.superclass.constructor.apply(this,arguments)};blockhider.prototype={initializer:function(config){this.set('block','#'+this.get('id'));var b=this.get('block'),t=b.one('.title'),a=null,hide,show;if(t&&(a=t.one('.block_action'))){hide=Y.Node.create('<img />').addClass('block-hider-hide').setAttrs({alt:config.tooltipVisible,src:this.get('iconVisible'),tabindex:0,title:config.tooltipVisible});hide.on('keypress',this.updateStateKey,this,true);hide.on('click',this.updateState,this,true);show=Y.Node.create('<img />').addClass('block-hider-show').setAttrs({alt:config.tooltipHidden,src:this.get('iconHidden'),tabindex:0,title:config.tooltipHidden});show.on('keypress',this.updateStateKey,this,false);show.on('click',this.updateState,this,false);a.insert(show,0).insert(hide,0)}},updateState:function(e,hide){M.util.set_user_preference(this.get('preference'),hide);if(hide){this.get('block').addClass('hidden')}else this.get('block').removeClass('hidden')},updateStateKey:function(e,hide){if(e.keyCode==13)this.updateState(this,hide)}};Y.extend(blockhider,Y.Base,blockhider.prototype,{NAME:'blockhider',ATTRS:{id:{},preference:{},iconVisible:{value:M.util.image_url('t/switch_minus','moodle')},iconHidden:{value:M.util.image_url('t/switch_plus','moodle')},block:{setter:function(node){return Y.one(node)}}}});return blockhider})();new M.util.block_hider(config)})};M.util.pending_js=[];M.util.complete_js=[];M.util.js_pending=function(uniqid){if(uniqid!==false)M.util.pending_js.push(uniqid);return M.util.pending_js.length};M.util.js_pending('init');YUI.add('moodle-core-io',function(Y){Y.on('io:start',function(id){M.util.js_pending('io:'+id)});Y.on('io:end',function(id){M.util.js_complete('io:'+id)})},'@VERSION@',{condition:{trigger:'io-base',when:'after'}});M.util.js_complete=function(uniqid){var index=Y.Array.indexOf(M.util.pending_js,uniqid);if(index>=0)M.util.complete_js.push(M.util.pending_js.splice(index,1));return M.util.pending_js.length};M.util.get_string=function(identifier,component,a){var stringvalue;if(M.cfg.developerdebug){if(typeof M.util.get_string_yui_instance==='undefined')M.util.get_string_yui_instance=new YUI({debug:true});var Y=M.util.get_string_yui_instance};if(!M.str.hasOwnProperty(component)||!M.str[component].hasOwnProperty(identifier)){stringvalue='[['+identifier+','+component+']]';if(M.cfg.developerdebug)Y.log('undefined string '+stringvalue,'warn','M.util.get_string');return stringvalue};stringvalue=M.str[component][identifier];if(typeof a=='undefined')return stringvalue;if(typeof a=='number'||typeof a=='string'){stringvalue=stringvalue.replace(/\{\$a\}/g,a);return stringvalue};if(typeof a=='object'){for(var key in a){if(typeof a[key]!='number'&&typeof a[key]!='string'){if(M.cfg.developerdebug)Y.log('invalid value type for $a->'+key,'warn','M.util.get_string');continue};var search='{$a->'+key+'}';search=search.replace(/[-[\]{}()*+?.,\\^$|#\s]/g,'\\$&');search=new RegExp(search,'g');stringvalue=stringvalue.replace(search,a[key])};return stringvalue};if(M.cfg.developerdebug)Y.log('incorrect placeholder type','warn','M.util.get_string');return stringvalue};M.util.focus_login_form=function(Y){var username=Y.one('#username'),password=Y.one('#password');if(username==null||password==null)return;var curElement=document.activeElement;if(curElement=='undefined');else if(curElement.tagName=='INPUT')return;if(username.get('value')==''){username.focus()}else password.focus()};M.util.focus_login_error=function(Y){var errorlog=Y.one('#loginerrormessage');if(errorlog)errorlog.focus()};M.util.add_lightbox=function(Y,node){var WAITICON={pix:"i/loading_small",component:'moodle'};if(node.one('.lightbox'))return node.one('.lightbox');node.setStyle('position','relative');var waiticon=Y.Node.create('<img />').setAttrs({src:M.util.image_url(WAITICON.pix,WAITICON.component)}).setStyles({position:'relative',top:'50%'}),lightbox=Y.Node.create('<div></div>').setStyles({opacity:'.75',position:'absolute',width:'100%',height:'100%',top:0,left:0,backgroundColor:'white',textAlign:'center'}).setAttribute('class','lightbox').hide();lightbox.appendChild(waiticon);node.append(lightbox);return lightbox};M.util.add_spinner=function(Y,node){var WAITICON={pix:"i/loading_small",component:'moodle'};if(node.one('.spinner'))return node.one('.spinner');var spinner=Y.Node.create('<img />').setAttribute('src',M.util.image_url(WAITICON.pix,WAITICON.component)).addClass('spinner').addClass('iconsmall').hide();node.append(spinner);return spinner}
function checkall(){var inputs=document.getElementsByTagName('input');for(var i=0;i<inputs.length;i++)if(inputs[i].type=='checkbox'){if(inputs[i].disabled||inputs[i].readOnly)continue;inputs[i].checked=true}}
function checknone(){var inputs=document.getElementsByTagName('input');for(var i=0;i<inputs.length;i++)if(inputs[i].type=='checkbox'){if(inputs[i].disabled||inputs[i].readOnly)continue;inputs[i].checked=false}}
function select_all_in_element_with_id(id,checked){var container=document.getElementById(id);if(!container)return;var inputs=container.getElementsByTagName('input');for(var i=0;i<inputs.length;++i)if(inputs[i].type=='checkbox'||inputs[i].type=='radio')inputs[i].checked=checked}
function select_all_in(elTagName,elClass,elId){var inputs=document.getElementsByTagName('input');inputs=filterByParent(inputs,function(el){return findParentNode(el,elTagName,elClass,elId)});for(var i=0;i<inputs.length;++i)if(inputs[i].type=='checkbox'||inputs[i].type=='radio')inputs[i].checked='checked'}
function deselect_all_in(elTagName,elClass,elId){var inputs=document.getElementsByTagName('INPUT');inputs=filterByParent(inputs,function(el){return findParentNode(el,elTagName,elClass,elId)});for(var i=0;i<inputs.length;++i)if(inputs[i].type=='checkbox'||inputs[i].type=='radio')inputs[i].checked=''}
function confirm_if(expr,message){if(!expr)return true;return confirm(message)}
function findParentNode(el,elName,elClass,elId){while(el.nodeName.toUpperCase()!='BODY'){if((!elName||el.nodeName.toUpperCase()==elName)&&(!elClass||el.className.indexOf(elClass)!=-1)&&(!elId||el.id==elId))break;el=el.parentNode};return el}
function findChildNodes(start,tagName,elementClass,elementID,elementName){Y.log("findChildNodes() is deprecated. Please use Y.all instead.","warn","javascript-static.js");var children=new Array();for(var i=0;i<start.childNodes.length;i++){var classfound=false,child=start.childNodes[i];if((child.nodeType==1)&&(elementClass&&(typeof(child.className)=='string'))){var childClasses=child.className.split(/\s+/);for(var childClassIndex in childClasses)if(childClasses[childClassIndex]==elementClass){classfound=true;break}};if(child.nodeType==1)if((!tagName||child.nodeName==tagName)&&(!elementClass||classfound)&&(!elementID||child.id==elementID)&&(!elementName||child.name==elementName)){children=children.concat(child)}else children=children.concat(findChildNodes(child,tagName,elementClass,elementID,elementName))};return children}
function unmaskPassword(id){var pw=document.getElementById(id),chb=document.getElementById(id+'unmask');if(Y.UA.ie==0||Y.UA.ie>=9){if(chb.checked){pw.type="text"}else pw.type="password"}else{try{if(chb.checked){var newpw=document.createElement('<input type="text" autocomplete="off" name="'+pw.name+'">')}else var newpw=document.createElement('<input type="password" autocomplete="off" name="'+pw.name+'">');newpw.attributes['class'].nodeValue=pw.attributes['class'].nodeValue}catch(e){var newpw=document.createElement('input');newpw.setAttribute('autocomplete','off');newpw.setAttribute('name',pw.name);if(chb.checked){newpw.setAttribute('type','text')}else newpw.setAttribute('type','password');newpw.setAttribute('class',pw.getAttribute('class'))};newpw.id=pw.id;newpw.size=pw.size;newpw.onblur=pw.onblur;newpw.onchange=pw.onchange;newpw.value=pw.value;pw.parentNode.replaceChild(newpw,pw)}}
function filterByParent(elCollection,parentFinder){var filteredCollection=[];for(var i=0;i<elCollection.length;++i){var findParent=parentFinder(elCollection[i]);if(findParent.nodeName.toUpperCase()!='BODY')filteredCollection.push(elCollection[i])};return filteredCollection}
function fix_column_widths(){var agt=navigator.userAgent.toLowerCase();if((agt.indexOf("msie")!=-1)&&(agt.indexOf("opera")==-1)){fix_column_width('left-column');fix_column_width('right-column')}}
function fix_column_width(colName){if(column=document.getElementById(colName)){if(!column.offsetWidth){setTimeout("fix_column_width('"+colName+"')",20);return};var width=0,nodes=column.childNodes;for(i=0;i<nodes.length;++i)if(nodes[i].className.indexOf("block")!=-1)if(width<nodes[i].offsetWidth)width=nodes[i].offsetWidth;for(i=0;i<nodes.length;++i)if(nodes[i].className.indexOf("block")!=-1)nodes[i].style.width=width+'px'}}
function insertAtCursor(myField,myValue){if(document.selection){myField.focus();sel=document.selection.createRange();sel.text=myValue}else if(myField.selectionStart||myField.selectionStart=='0'){var startPos=myField.selectionStart,endPos=myField.selectionEnd;myField.value=myField.value.substring(0,startPos)+myValue+myField.value.substring(endPos,myField.value.length)}else myField.value+=myValue}
function addonload(fn){Y.log('addonload has been deprecated since Moodle 2.7 and will be removed in Moodle 2.9','warn','javascript-static.js');var oldhandler=window.onload;window.onload=function(){if(oldhandler)oldhandler();fn()}}
function getElementsByClassName(oElm,strTagName,name){Y.log('getElementsByClassName has been deprecated since Moodle 2.7 and will be removed in Moodle 2.9','warn','javascript-static.js');if(typeof name=="object"){var names=new Array();for(var i=0;i<name.length;i++)names.push(names[i]);name=names.join('')};if(oElm.getElementsByClassName&&Array.filter)if(strTagName=='*'){return oElm.getElementsByClassName(name)}else return Array.filter(oElm.getElementsByClassName(name),function(el){return el.nodeName.toLowerCase()==strTagName.toLowerCase()});var arrElements=(strTagName=="*"&&oElm.all)?oElm.all:oElm.getElementsByTagName(strTagName),arrReturnElements=new Array(),arrRegExpClassNames=new Array(),names=name.split(' ');for(var i=0;i<names.length;i++)arrRegExpClassNames.push(new RegExp("(^|\\s)"+names[i].replace(/\-/g,"\\-")+"(\\s|$)"));var oElement,bMatchesAll;for(var j=0;j<arrElements.length;j++){oElement=arrElements[j];bMatchesAll=true;for(var k=0;k<arrRegExpClassNames.length;k++)if(!arrRegExpClassNames[k].test(oElement.className)){bMatchesAll=false;break};if(bMatchesAll)arrReturnElements.push(oElement)};return arrReturnElements}
function increment_filename(filename,ignoreextension){var extension='',basename=filename;if(!ignoreextension){var dotpos=filename.lastIndexOf('.');if(dotpos!==-1){basename=filename.substr(0,dotpos);extension=filename.substr(dotpos,filename.length)}};var number=0,hasnumber=basename.match(/^(.*) \((\d+)\)$/);if(hasnumber!==null){number=parseInt(hasnumber[2],10);basename=hasnumber[1]};number++;var newname=basename+' ('+number+')'+extension;return newname}
function right_to_left(){var body=Y.one('body'),rtl=false;if(body&&body.hasClass('dir-rtl'))rtl=true;return rtl}
function openpopup(event,args){if(event)if(event.preventDefault){event.preventDefault()}else event.returnValue=false;var nameregex=/[^a-z0-9_]/i;if(typeof args.name!=='string'){args.name='_blank'}else if(args.name.match(nameregex)){if(M.cfg.developerdebug)alert('DEVELOPER NOTICE: Invalid \'name\' passed to openpopup(): '+args.name);args.name=args.name.replace(nameregex,'_')};var fullurl=args.url;if(!args.url.match(/https?:\/\//))fullurl=M.cfg.wwwroot+args.url;if(args.fullscreen)args.options=args.options.replace(/top=\d+/,'top=0').replace(/left=\d+/,'left=0').replace(/width=\d+/,'width='+screen.availWidth).replace(/height=\d+/,'height='+screen.availHeight);var windowobj=window.open(fullurl,args.name,args.options);if(!windowobj)return true;if(args.fullscreen){var hackcount=100,get_size_exactly_right=function(){windowobj.moveTo(0,0);windowobj.resizeTo(screen.availWidth,screen.availHeight);if(hackcount>0&&(windowobj.innerHeight<10||windowobj.innerWidth<10)){hackcount-=1;setTimeout(get_size_exactly_right,10)}};setTimeout(get_size_exactly_right,0)};windowobj.focus();return false}
function close_window(e){if(e.preventDefault){e.preventDefault()}else e.returnValue=false;window.close()}
function show_item(itemid){Y.log('show_item has been deprecated since Moodle 2.7 and will be removed in Moodle 2.9','warn','javascript-static.js');var item=Y.one('#'+itemid);if(item)item.show()}
function destroy_item(itemid){Y.log('destroy_item has been deprecated since Moodle 2.7 and will be removed in Moodle 2.9','warn','javascript-static.js');var item=Y.one('#'+itemid);if(item)item.remove(true)}
function focuscontrol(controlid){var control=document.getElementById(controlid);if(control)control.focus()}
function old_onload_focus(formid,controlname){if(document.forms[formid]&&document.forms[formid].elements&&document.forms[formid].elements[controlname])document.forms[formid].elements[controlname].focus()}
function build_querystring(obj){return convert_object_to_string(obj,'&')}
function build_windowoptionsstring(obj){return convert_object_to_string(obj,',')}
function convert_object_to_string(obj,separator){if(typeof obj!=='object')return null;var list=[];for(var k in obj){k=encodeURIComponent(k);var value=obj[k];if(obj[k]instanceof Array){for(var i in value)list.push(k+'[]='+encodeURIComponent(value[i]))}else list.push(k+'='+encodeURIComponent(value))};return list.join(separator)}
function stripHTML(str){var re=/<\S[^><]*>/g,ret=str.replace(re,"");return ret};Number.prototype.fixed=function(n){with(Math)return round(Number(this)*pow(10,n))/pow(10,n)}
function update_progress_bar(id,width,pt,msg,es){var percent=pt,status=document.getElementById("status_"+id),percent_indicator=document.getElementById("pt_"+id),progress_bar=document.getElementById("progress_"+id),time_es=document.getElementById("time_"+id);status.innerHTML=msg;percent_indicator.innerHTML=percent.fixed(2)+'%';if(percent==100){progress_bar.style.background="green";time_es.style.display="none"}else{progress_bar.style.background="#FFCC66";if(es=='?'){time_es.innerHTML=""}else{time_es.innerHTML=es.fixed(2)+" sec";time_es.style.display="block"}};progress_bar.style.width=width+"px"}
function hide_item(itemid){Y.log('hide_item has been deprecated since Moodle 2.7 and will be removed in Moodle 2.9','warn','javascript-static.js');var item=Y.one('#'+itemid);if(item)item.hide()};M.util.help_popups={setup:function(Y){Y.one('body').delegate('click',this.open_popup,'a.helplinkpopup',this)},open_popup:function(e){e.preventDefault();var anchor=e.target.ancestor('a',true),args={name:'popup',url:anchor.getAttribute('href'),options:''},options=['height=600','width=800','top=0','left=0','menubar=0','location=0','scrollbars','resizable','toolbar','status','directories=0','fullscreen=0','dependent'];args.options=options.join(',');openpopup(e,args)}};M.core_custom_menu={init:function(Y,nodeid){var node=Y.one('#'+nodeid);if(node)Y.use('node-menunav',function(Y){node.removeClass('javascript-disabled');node.plug(Y.Plugin.NodeMenuNav)})}};M.form=M.form||{};M.form.init_smartselect=function(Y,id,options){if(!id.match(/^id_/))id='id_'+id;var select=Y.one('select#'+id);if(!select)return false;Y.use('event-delegate',function(){var smartselect={id:id,structure:[],options:[],submenucount:0,currentvalue:null,currenttext:null,shownevent:null,cfg:{selectablecategories:true,mode:null},nodes:{select:null,loading:null,menu:null},init:function(Y,id,args,nodes){if(typeof args=='object')for(var i in this.cfg)if(args[i]||args[i]===false)this.cfg[i]=args[i];this.nodes.select=nodes.select;this.currentvalue=this.nodes.select.get('selectedIndex');this.currenttext=this.nodes.select.all('option').item(this.currentvalue).get('innerHTML');var options=Array();options['']={text:this.currenttext,value:'',depth:0,children:[]};this.nodes.select.all('option').each(function(option,index){var rawtext=option.get('innerHTML'),text=rawtext.replace(/^(&nbsp;)*/,'');if(rawtext===text){text=rawtext.replace(/^(\s)*/,'');var depth=(rawtext.length-text.length)+1}else var depth=((rawtext.length-text.length)/12)+1;option.set('innerHTML',text);options['i'+index]={text:text,depth:depth,index:index,children:[]}},this);this.structure=[];var structcount=0;for(var i in options){var o=options[i];if(o.depth==0){this.structure.push(o);structcount++}else{var d=o.depth,current=this.structure[structcount-1];for(var j=0;j<o.depth-1;j++)if(current&&current.children)current=current.children[current.children.length-1];if(current&&current.children)current.children.push(o)}};this.nodes.menu=Y.Node.create(this.generate_menu_content());this.nodes.menu.one('.smartselect_mask').setStyle('opacity',0.01);this.nodes.menu.one('.smartselect_mask').setStyle('width',(this.nodes.select.get('offsetWidth')+5)+'px');this.nodes.menu.one('.smartselect_mask').setStyle('height',(this.nodes.select.get('offsetHeight'))+'px');if(this.cfg.mode==null){var formwidth=this.nodes.select.ancestor('form').get('offsetWidth');if(formwidth<400||this.nodes.menu.get('offsetWidth')<formwidth*2){this.cfg.mode='compact'}else this.cfg.mode='spanning'};if(this.cfg.mode=='compact'){this.nodes.menu.addClass('compactmenu')}else{this.nodes.menu.addClass('spanningmenu');this.nodes.menu.delegate('mouseover',this.show_sub_menu,'.smartselect_submenuitem',this)};Y.one(document.body).append(this.nodes.menu);var pos=this.nodes.select.getXY();pos[0]+=1;this.nodes.menu.setXY(pos);this.nodes.menu.on('click',this.handle_click,this);Y.one(window).on('resize',function(){var pos=this.nodes.select.getXY();pos[0]+=1;this.nodes.menu.setXY(pos)},this)},generate_menu_content:function(){var content='<div id="'+this.id+'_smart_select" class="smartselect">';content+=this.generate_submenu_content(this.structure[0],true);content+='</ul></div>';return content},generate_submenu_content:function(item,rootelement){this.submenucount++;var content='';if(item.children.length>0){if(rootelement){content+='<div class="smartselect_mask" href="#ss_submenu'+this.submenucount+'">&nbsp;</div>';content+='<div id="ss_submenu'+this.submenucount+'" class="smartselect_menu">';content+='<div class="smartselect_menu_content">'}else{content+='<li class="smartselect_submenuitem">';var categoryclass=(this.cfg.selectablecategories)?'selectable':'notselectable';content+='<a class="smartselect_menuitem_label '+categoryclass+'" href="#ss_submenu'+this.submenucount+'" value="'+item.index+'">'+item.text+'</a>';content+='<div id="ss_submenu'+this.submenucount+'" class="smartselect_submenu">';content+='<div class="smartselect_submenu_content">'};content+='<ul>';for(var i in item.children)content+=this.generate_submenu_content(item.children[i],false);content+='</ul>';content+='</div>';content+='</div>';if(rootelement);else content+='</li>'}else{content+='<li class="smartselect_menuitem">';content+='<a class="smartselect_menuitem_content selectable" href="#" value="'+item.index+'">'+item.text+'</a>';content+='</li>'};return content},select:function(e){var t=e.target;e.halt();this.currenttext=t.get('innerHTML');this.currentvalue=t.getAttribute('value');this.nodes.select.set('selectedIndex',this.currentvalue);this.hide_menu()},handle_click:function(e){var target=e.target;if(target.hasClass('smartselect_mask')){this.show_menu(e)}else if(target.hasClass('selectable')||target.hasClass('smartselect_menuitem')){this.select(e)}else if(target.hasClass('smartselect_menuitem_label')||target.hasClass('smartselect_submenuitem'))this.show_sub_menu(e)},show_menu:function(e){e.halt();var menu=e.target.ancestor().one('.smartselect_menu');menu.addClass('visible');this.shownevent=Y.one(document.body).on('click',this.hide_menu,this)},show_sub_menu:function(e){e.halt();var target=e.target;if(!target.hasClass('smartselect_submenuitem'))target=target.ancestor('.smartselect_submenuitem');if(this.cfg.mode=='compact'&&target.one('.smartselect_submenu').hasClass('visible')){target.ancestor('ul').all('.smartselect_submenu.visible').removeClass('visible');return};target.ancestor('ul').all('.smartselect_submenu.visible').removeClass('visible');target.one('.smartselect_submenu').addClass('visible')},hide_menu:function(){this.nodes.menu.all('.visible').removeClass('visible');if(this.shownevent)this.shownevent.detach()}};smartselect.init(Y,id,options,{select:select})})};M.util.video_players=[];M.util.audio_players=[];M.util.add_video_player=function(id,fileurl,width,height,autosize){M.util.video_players.push({id:id,fileurl:fileurl,width:width,height:height,autosize:autosize,resized:false})};M.util.add_audio_player=function(id,fileurl,small){M.util.audio_players.push({id:id,fileurl:fileurl,small:small})};M.util.load_flowplayer=function(){if(M.util.video_players.length==0&&M.util.audio_players.length==0)return;if(typeof flowplayer=='undefined'){var loaded=false,embed_function=function(){if(loaded||typeof flowplayer=='undefined')return;loaded=true;var controls={autoHide:true};for(var i=0;i<M.util.video_players.length;i++){var video=M.util.video_players[i];if(video.width>0&&video.height>0){var src={src:M.cfg.wwwroot+'/lib/flowplayer/flowplayer-3.2.18.swf',width:video.width,height:video.height}}else var src=M.cfg.wwwroot+'/lib/flowplayer/flowplayer-3.2.18.swf';flowplayer(video.id,src,{plugins:{controls:controls},clip:{url:video.fileurl,autoPlay:false,autoBuffering:true,scaling:'fit',mvideo:video,onMetaData:function(clip){if(clip.mvideo.autosize&&!clip.mvideo.resized){clip.mvideo.resized=true;if(typeof(clip.metaData.width)=='undefined'||typeof(clip.metaData.height)=='undefined'){var width=clip.width,height=clip.height}else{var width=clip.metaData.width,height=clip.metaData.height};var minwidth=300;if(width<minwidth){height=(height*minwidth)/width;width=minwidth};var object=this._api();object.width=width;object.height=height}}}})};if(M.util.audio_players.length==0)return;var controls={autoHide:false,fullscreen:false,next:false,previous:false,scrubber:true,play:true,pause:true,volume:true,mute:false,backgroundGradient:[0.5,0,0.3]},rule;for(var j=0;j<document.styleSheets.length;j++){var allrules=false;try{if(typeof(document.styleSheets[j].rules)!='undefined'){allrules=document.styleSheets[j].rules}else if(typeof(document.styleSheets[j].cssRules)!='undefined'){allrules=document.styleSheets[j].cssRules}else continue}catch(e){continue};if(!allrules)continue;for(var i=0;i<allrules.length;i++){rule='';if(/^\.mp3flowplayer_.*Color$/.test(allrules[i].selectorText)){if(typeof(allrules[i].cssText)!='undefined'){rule=allrules[i].cssText}else if(typeof(allrules[i].style.cssText)!='undefined')rule=allrules[i].style.cssText;if(rule!=''&&/.*color\s*:\s*([^;]+).*/gi.test(rule)){rule=rule.replace(/.*color\s*:\s*([^;]+).*/gi,'$1');var colprop=allrules[i].selectorText.replace(/^\.mp3flowplayer_/,'');controls[colprop]=rule}}};allrules=false};for(i=0;i<M.util.audio_players.length;i++){var audio=M.util.audio_players[i];if(audio.small){controls.controlall=false;controls.height=15;controls.time=false}else{controls.controlall=true;controls.height=25;controls.time=true};flowplayer(audio.id,M.cfg.wwwroot+'/lib/flowplayer/flowplayer-3.2.18.swf',{plugins:{controls:controls,audio:{url:M.cfg.wwwroot+'/lib/flowplayer/flowplayer.audio-3.2.11.swf'}},clip:{url:audio.fileurl,provider:"audio",autoPlay:false}})}};if(M.cfg.jsrev==-1){var jsurl=M.cfg.wwwroot+'/lib/flowplayer/flowplayer-3.2.13.js'}else var jsurl=M.cfg.wwwroot+'/lib/javascript.php?jsfile=/lib/flowplayer/flowplayer-3.2.13.min.js&rev='+M.cfg.jsrev;var fileref=document.createElement('script');fileref.setAttribute('type','text/javascript');fileref.setAttribute('src',jsurl);fileref.onload=embed_function;fileref.onreadystatechange=embed_function;document.getElementsByTagName('head')[0].appendChild(fileref)}}