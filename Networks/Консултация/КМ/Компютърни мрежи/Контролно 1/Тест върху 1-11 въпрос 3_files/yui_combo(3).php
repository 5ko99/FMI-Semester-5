/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-synthetic",function(e,t){function c(e,t){this.handle=e,this.emitFacade=t}function h(e,t,n){this.handles=[],this.el=e,this.key=n,this.domkey=t}function p(){this._init.apply(this,arguments)}var n=e.CustomEvent,r=e.Env.evt.dom_map,i=e.Array,s=e.Lang,o=s.isObject,u=s.isString,a=s.isArray,f=e.Selector.query,l=function(){};c.prototype.fire=function(t){var n=i(arguments,0,!0),r=this.handle,s=r.evt,u=r.sub,a=u.context,f=u.filter,l=t||{},c;if(this.emitFacade){if(!t||!t.preventDefault)l=s._getFacade(),o(t)&&!t.preventDefault?(e.mix(l,t,!0),n[0]=l):n.unshift(l);l.type=s.type,l.details=n.slice(),f&&(l.container=s.host)}else f&&o(t)&&t.currentTarget&&n.shift();return u.context=a||l.currentTarget||s.host,c=s.fire.apply(s,n),u.context=a,c},h.prototype={constructor:h,type:"_synth",fn:l,capture:!1,register:function(e){e.evt.registry=this,this.handles.push(e)},unregister:function(t){var n=this.handles,i=r[this.domkey],s;for(s=n.length-1;s>=0;--s)if(n[s].sub===t){n.splice(s,1);break}n.length||(delete i[this.key],e.Object.size(i)||delete r[this.domkey])},detachAll:function(){var e=this.handles,t=e.length;while(--t>=0)e[t].detach()}},e.mix(p,{Notifier:c,SynthRegistry:h,getRegistry:function(t,n,i){var s=t._node,o=e.stamp(s),u="event:"+o+n+"_synth",a=r[o];return i&&(a||(a=r[o]={}),a[u]||(a[u]=new h(s,o,u))),a&&a[u]||null},_deleteSub:function(e){if(e&&e.fn){var t=this.eventDef,r=e.filter?"detachDelegate":"detach";this._subscribers=[],n.keepDeprecatedSubs&&(this.subscribers={}),t[r](e.node,e,this.notifier,e.filter),this.registry.unregister(e),delete e.fn,delete e.node,delete e.context}},prototype:{constructor:p,_init:function(){var e=this.publishConfig||(this.publishConfig={});this.emitFacade="emitFacade"in e?e.emitFacade:!0,e.emitFacade=!1},processArgs:l,on:l,detach:l,delegate:l,detachDelegate:l,_on:function(t,n){var r=[],s=t.slice(),o=this.processArgs(t,n),a=t[2],l=n?"delegate":"on",c,h;return c=u(a)?f(a):i(a||e.one(e.config.win)),!c.length&&u(a)?(h=e.on("available",function(){e.mix(h,e[l].apply(e,s),!0)},a),h):(e.Array.each(c,function(i){var s=t.slice(),u;i=e.one(i),i&&(n&&(u=s.splice(3,1)[0]),s.splice(0,4,s[1],s[3]),(!this.preventDups||!this.getSubs(i,t,null,!0))&&r.push(this._subscribe(i,l,s,o,u)))},this),r.length===1?r[0]:new e.EventHandle(r))},_subscribe:function(t,n,r,i,s){var o=new e.CustomEvent(this.type,this.publishConfig),u=o.on.apply(o,r),a=new c(u,this.emitFacade),f=p.getRegistry(t,this.type,!0),l=u.sub;return l.node=t,l.filter=s,i&&this.applyArgExtras(i,l),e.mix(o,{eventDef:this,notifier:a,host:t,currentTarget:t,target:t,el:t._node,_delete:p._deleteSub},!0),u.notifier=a,f.register(u),this[n](t,l,a,s),u},applyArgExtras:function(e,t){t._extra=e},_detach:function(t){var n=t[2],r=u(n)?f(n):i(n),s,o,a,l,c;t.splice(2,1);for(o=0,a=r.length;o<a;++o){s=e.one(r[o]);if(s){l=this.getSubs(s,t);if(l)for(c=l.length-1;c>=0;--c)l[c].detach()}}},getSubs:function(e,t,n,r){var i=p.getRegistry(e,this.type),s=[],o,u,a,f;if(i){o=i.handles,n||(n=this.subMatch);for(u=0,a=o.length;u<a;++u){f=o[u];if(n.call(this,f.sub,t)){if(r)return f;s.push(o[u])}}}return s.length&&s},subMatch:function(e,t){return!t[1]||e.fn===t[1]}}},!0),e.SyntheticEvent=p,e.Event.define=function(t,n,r){var s,o,f;t&&t.type?(s=t,r=n):n&&(s=e.merge({type:t},n));if(s){if(r||!e.Node.DOM_EVENTS[s.type])o=function(){p.apply(this,arguments)},e.extend(o,p,s),f=new o,t=f.type,e.Node.DOM_EVENTS[t]=e.Env.evt.plugins[t]={eventDef:f,on:function(){return f._on(i(arguments))},delegate:function(){return f._on(i(arguments),!0)},detach:function(){return f._detach(i(arguments))}}}else(u(t)||a(t))&&e.Array.each(i(t),function(t){e.Node.DOM_EVENTS[t]=1});return f}},"3.7.3",{requires:["node-base","event-custom-complex"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-key",function(e,t){var n="+alt",r="+ctrl",i="+meta",s="+shift",o=e.Lang.trim,u={KEY_MAP:{enter:13,esc:27,backspace:8,tab:9,pageup:33,pagedown:34},_typeRE:/^(up|down|press):/,_keysRE:/^(?:up|down|press):|\+(alt|ctrl|meta|shift)/g,processArgs:function(t){var n=t.splice(3,1)[0],r=e.Array.hash(n.match(/\+(?:alt|ctrl|meta|shift)\b/g)||[]),i={type:this._typeRE.test(n)?RegExp.$1:null,mods:r,keys:null},s=n.replace(this._keysRE,""),u,a,f,l;if(s){s=s.split(","),i.keys={};for(l=s.length-1;l>=0;--l){u=o(s[l]);if(!u)continue;+u==u?i.keys[u]=r:(f=u.toLowerCase(),this.KEY_MAP[f]?(i.keys[this.KEY_MAP[f]]=r,i.type||(i.type="down")):(u=u.charAt(0),a=u.toUpperCase(),r["+shift"]&&(u=a),i.keys[u.charCodeAt(0)]=u===a?e.merge(r,{"+shift":!0}):r))}}return i.type||(i.type="press"),i},on:function(e,t,o,u){var a=t._extra,f="key"+a.type,l=a.keys,c=u?"delegate":"on";t._detach=e[c](f,function(e){var t=l?l[e.which]:a.mods;t&&(!t[n]||t[n]&&e.altKey)&&(!t[r]||t[r]&&e.ctrlKey)&&(!t[i]||t[i]&&e.metaKey)&&(!t[s]||t[s]&&e.shiftKey)&&o.fire(e)},u)},detach:function(e,t,n){t._detach.detach()}};u.delegate=u.on,u.detachDelegate=u.detach,e.Event.define("key",u,!0)},"3.7.3",{requires:["event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-mousewheel",function(e,t){var n="DOMMouseScroll",r=function(t){var r=e.Array(t,0,!0),i;return e.UA.gecko?(r[0]=n,i=e.config.win):i=e.config.doc,r.length<3?r[2]=i:r.splice(2,0,i),r};e.Env.evt.plugins.mousewheel={on:function(){return e.Event._attach(r(arguments))},detach:function(){return e.Event.detach.apply(e.Event,r(arguments))}}},"3.7.3",{requires:["node-base"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-mouseenter",function(e,t){var n=e.Env.evt.dom_wrappers,r=e.DOM.contains,i=e.Array,s=function(){},o={proxyType:"mouseover",relProperty:"fromElement",_notify:function(t,i,s){var o=this._node,u=t.relatedTarget||t[i];o!==u&&!r(o,u)&&s.fire(new e.DOMEventFacade(t,o,n["event:"+e.stamp(o)+t.type]))},on:function(t,n,r){var i=e.Node.getDOMNode(t),s=[this.proxyType,this._notify,i,null,this.relProperty,r];n.handle=e.Event._attach(s,{facade:!1})},detach:function(e,t){t.handle.detach()},delegate:function(t,n,r,i){var o=e.Node.getDOMNode(t),u=[this.proxyType,s,o,null,r];n.handle=e.Event._attach(u,{facade:!1}),n.handle.sub.filter=i,n.handle.sub.relProperty=this.relProperty,n.handle.sub._notify=this._filterNotify},_filterNotify:function(t,n,s){n=n.slice(),this.args&&n.push.apply(n,this.args);var o=e.delegate._applyFilter(this.filter,n,s),u=n[0].relatedTarget||n[0][this.relProperty],a,f,l,c,h;if(o){o=i(o);for(f=0,l=o.length&&(!a||!a.stopped);f<l;++f){h=o[0];if(!r(h,u)){a||(a=new e.DOMEventFacade(n[0],h,s),a.container=e.one(s.el)),a.currentTarget=e.one(h),c=n[1].fire(a);if(c===!1)break}}}return c},detachDelegate:function(e,t){t.handle.detach()}};e.Event.define("mouseenter",o,!0),e.Event.define("mouseleave",e.merge(o,{proxyType:"mouseout",relProperty:"toElement"}),!0)},"3.7.3",{requires:["event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-focus",function(e,t){function u(t,r,u){var a="_"+t+"Notifiers";e.Event.define(t,{_useActivate:o,_attach:function(i,s,o){return e.DOM.isWindow(i)?n._attach([t,function(e){s.fire(e)},i]):n._attach([r,this._proxy,i,this,s,o],{capture:!0})},_proxy:function(t,r,i){var s=t.target,f=t.currentTarget,l=s.getData(a),c=e.stamp(f._node),h=o||s!==f,p;r.currentTarget=i?s:f,r.container=i?f:null,l?h=!0:(l={},s.setData(a,l),h&&(p=n._attach([u,this._notify,s._node]).sub,p.once=!0)),l[c]||(l[c]=[]),l[c].push(r),h||this._notify(t)},_notify:function(t,n){var r=t.currentTarget,i=r.getData(a),o=r.ancestors(),u=r.get("ownerDocument"),f=[],l=i?e.Object.keys(i).length:0,c,h,p,d,v,m,g,y,b,w;r.clearData(a),o.push(r),u&&o.unshift(u),o._nodes.reverse(),l&&(m=l,o.some(function(t){var n=e.stamp(t),r=i[n],s,o;if(r){l--;for(s=0,o=r.length;s<o;++s)r[s].handle.sub.filter&&f.push(r[s])}return!l}),l=m);while(l&&(c=o.shift())){d=e.stamp(c),h=i[d];if(h){for(g=0,y=h.length;g<y;++g){p=h[g],b=p.handle.sub,v=!0,t.currentTarget=c,b.filter&&(v=b.filter.apply(c,[c,t].concat(b.args||[])),f.splice(s(f,p),1)),v&&(t.container=p.container,w=p.fire(t));if(w===!1||t.stopped===2)break}delete h[d],l--}if(t.stopped!==2)for(g=0,y=f.length;g<y;++g){p=f[g],b=p.handle.sub,b.filter.apply(c,[c,t].concat(b.args||[]))&&(t.container=p.container,t.currentTarget=c,w=p.fire(t));if(w===!1||t.stopped===2)break}if(t.stopped)break}},on:function(e,t,n){t.handle=this._attach(e._node,n)},detach:function(e,t){t.handle.detach()},delegate:function(t,n,r,s){i(s)&&(n.filter=function(n){return e.Selector.test(n._node,s,t===n?null:t._node)}),n.handle=this._attach(t._node,r,!0)},detachDelegate:function(e,t){t.handle.detach()}},!0)}var n=e.Event,r=e.Lang,i=r.isString,s=e.Array.indexOf,o=function(){var t=e.config.doc.createElement("p"),n;return t.setAttribute("onbeforeactivate",";"),n=t.onbeforeactivate,n!==undefined}();o?(u("focus","beforeactivate","focusin"),u("blur","beforedeactivate","focusout")):(u("focus","focus","focus"),u("blur","blur","blur"))},"3.7.3",{requires:["event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-resize",function(e,t){e.Event.define("windowresize",{on:e.UA.gecko&&e.UA.gecko<1.91?function(t,n,r){n._handle=e.Event.attach("resize",function(e){r.fire(e)})}:function(t,n,r){var i=e.config.windowResizeDelay||100;n._handle=e.Event.attach("resize",function(t){n._timer&&n._timer.cancel(),n._timer=e.later(i,e,function(){r.fire(t)})})},detach:function(e,t){t._timer&&t._timer.cancel(),t._handle.detach()}})},"3.7.3",{requires:["node-base","event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-hover",function(e,t){var n=e.Lang.isFunction,r=function(){},i={processArgs:function(e){var t=n(e[2])?2:3;return n(e[t])?e.splice(t,1)[0]:r},on:function(e,t,n,r){var i=t.args?t.args.slice():[];i.unshift(null),t._detach=e[r?"delegate":"on"]({mouseenter:function(e){e.phase="over",n.fire(e)},mouseleave:function(e){var n=t.context||this;i[0]=e,e.type="hover",e.phase="out",t._extra.apply(n,i)}},r)},detach:function(e,t,n){t._detach.detach()}};i.delegate=i.on,i.detachDelegate=i.detach,e.Event.define("hover",i)},"3.7.3",{requires:["event-mouseenter"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-outside",function(e,t){var n=["blur","change","click","dblclick","focus","keydown","keypress","keyup","mousedown","mousemove","mouseout","mouseover","mouseup","select","submit"];e.Event.defineOutside=function(t,n){n=n||t+"outside";var r={on:function(n,r,i){r.handle=e.one("doc").on(t,function(e){this.isOutside(n,e.target)&&(e.currentTarget=n,i.fire(e))},this)},detach:function(e,t,n){t.handle.detach()},delegate:function(n,r,i,s){r.handle=e.one("doc").delegate(t,function(e){this.isOutside(n,e.target)&&i.fire(e)},s,this)},isOutside:function(e,t){return t!==e&&!t.ancestor(function(t){return t===e})}};r.detachDelegate=r.detach,e.Event.define(n,r)},e.Array.each(n,function(t){e.Event.defineOutside(t)})},"3.7.3",{requires:["event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-touch",function(e,t){var n="scale",r="rotation",i="identifier",s=e.config.win,o={};e.DOMEventFacade.prototype._touch=function(t,s,o){var u,a,f,l,c;if(t.touches){this.touches=[],c={};for(u=0,a=t.touches.length;u<a;++u)l=t.touches[u],c[e.stamp(l)]=this.touches[u]=new e.DOMEventFacade(l,s,o)}if(t.targetTouches){this.targetTouches=[];for(u=0,a=t.targetTouches.length;u<a;++u)l=t.targetTouches[u],f=c&&c[e.stamp(l,!0)],this.targetTouches[u]=f||new e.DOMEventFacade(l,s,o)}if(t.changedTouches){this.changedTouches=[];for(u=0,a=t.changedTouches.length;u<a;++u)l=t.changedTouches[u],f=c&&c[e.stamp(l,!0)],this.changedTouches[u]=f||new e.DOMEventFacade(l,s,o)}n in t&&(this[n]=t[n]),r in t&&(this[r]=t[r]),i in t&&(this[i]=t[i])},e.Node.DOM_EVENTS&&e.mix(e.Node.DOM_EVENTS,{touchstart:1,touchmove:1,touchend:1,touchcancel:1,gesturestart:1,gesturechange:1,gestureend:1,MSPointerDown:1,MSPointerUp:1,MSPointerMove:1}),s&&"ontouchstart"in s&&!(e.UA.chrome&&e.UA.chrome<6)?(o.start="touchstart",o.end="touchend",o.move="touchmove"):s&&"msPointerEnabled"in s.navigator?(o.start="MSPointerDown",o.end="MSPointerUp",o.move="MSPointerMove"):(o.start="mousedown",o.end="mouseup",o.move="mousemove"),e.Event._GESTURE_MAP=o},"3.7.3",{requires:["node-base"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-move",function(e,t){var n=e.Event._GESTURE_MAP,r={start:n.start,end:n.end,move:n.move},i="start",s="move",o="end",u="gesture"+s,a=u+o,f=u+i,l="_msh",c="_mh",h="_meh",p="_dmsh",d="_dmh",v="_dmeh",m="_ms",g="_m",y="minTime",b="minDistance",w="preventDefault",E="button",S="ownerDocument",x="currentTarget",T="target",N="nodeType",C=e.config.win&&"msPointerEnabled"in e.config.win.navigator,k="msTouchActionCount",L="msInitTouchAction",A=function(t,n,r){var i=r?4:3,s=n.length>i?e.merge(n.splice(i,1)[0]):{};return w in s||(s[w]=t.PREVENT_DEFAULT),s},O=function(e,t){return t._extra.root||e.get(N)===9?e:e.get(S)},M=function(t){var n=t.getDOMNode();return t.compareTo(e.config.doc)&&n.documentElement?n.documentElement:!1},_=function(e,t,n){e.pageX=t.pageX,e.pageY=t.pageY,e.screenX=t.screenX,e.screenY=t.screenY,e.clientX=t.clientX,e.clientY=t.clientY,e[T]=e[T]||t[T],e[x]=e[x]||t[x],e[E]=n&&n[E]||1},D=function(t){var n=M(t)||t.getDOMNode(),r=t.getData(k);C&&(r||(r=0,t.setData(L,n.style.msTouchAction)),n.style.msTouchAction=e.Event._DEFAULT_TOUCH_ACTION,r++,t.setData(k,r))},P=function(e){var t=M(e)||e.getDOMNode(),n=e.getData(k),r=e.getData(L);C&&(n--,e.setData(k,n),n===0&&t.style.msTouchAction!==r&&(t.style.msTouchAction=r))},H=function(e,t){t&&(!t.call||t(e))&&e.preventDefault()},B=e.Event.define;e.Event._DEFAULT_TOUCH_ACTION="none",B(f,{on:function(e,t,n){D(e),t[l]=e.on(r[i],this._onStart,this,e,t,n)},delegate:function(e,t,n,s){var o=this;t[p]=e.delegate(r[i],function(r){o._onStart(r,e,t,n,!0)},s)},detachDelegate:function(e,t,n,r){var i=t[p];i&&(i.detach(),t[p]=null),P(e)},detach:function(e,t,n){var r=t[l];r&&(r.detach(),t[l]=null),P(e)},processArgs:function(e,t){var n=A(this,e,t);return y in n||(n[y]=this.MIN_TIME),b in n||(n[b]=this.MIN_DISTANCE),n},_onStart:function(t,n,i,u,a){a&&(n=t[x]);var f=i._extra,l=!0,c=f[y],h=f[b],p=f.button,d=f[w],v=O(n,i),m;t.touches?t.touches.length===1?_(t,t.touches[0],f):l=!1:l=p===undefined||p===t.button,l&&(H(t,d),c===0||h===0?this._start(t,n,u,f):(m=[t.pageX,t.pageY],c>0&&(f._ht=e.later(c,this,this._start,[t,n,u,f]),f._hme=v.on(r[o],e.bind(function(){this._cancel(f)},this))),h>0&&(f._hm=v.on(r[s],e.bind(function(e){(Math.abs(e.pageX-m[0])>h||Math.abs(e.pageY-m[1])>h)&&this._start(t,n,u,f)},this)))))},_cancel:function(e){e._ht&&(e._ht.cancel(),e._ht=null),e._hme&&(e._hme.detach(),e._hme=null),e._hm&&(e._hm.detach(),e._hm=null)},_start:function(e,t,n,r){r&&this._cancel(r),e.type=f,t.setData(m,e),n.fire(e)},MIN_TIME:0,MIN_DISTANCE:0,PREVENT_DEFAULT:!1}),B(u,{on:function(e,t,n){D(e);var i=O(e,t,r[s]),o=i.on(r[s],this._onMove,this,e,t,n);t[c]=o},delegate:function(e,t,n,i){var o=this;t[d]=e.delegate(r[s],function(r){o._onMove(r,e,t,n,!0)},i)},detach:function(e,t,n){var r=t[c];r&&(r.detach(),t[c]=null),P(e)},detachDelegate:function(e,t,n,r){var i=t[d];i&&(i.detach(),t[d]=null),P(e)},processArgs:function(e,t){return A(this,e,t)},_onMove:function(e,t,n,r,i){i&&(t=e[x]);var s=n._extra.standAlone||t.getData(m),o=n._extra.preventDefault;s&&(e.touches&&(e.touches.length===1?_(e,e.touches[0]):s=!1),s&&(H(e,o),e.type=u,r.fire(e)))},PREVENT_DEFAULT:!1}),B(a,{on:function(e,t,n){D(e);var i=O(e,t),s=i.on(r[o],this._onEnd,this,e,t,n);t[h]=s},delegate:function(e,t,n,i){var s=this;t[v]=e.delegate(r[o],function(r){s._onEnd(r,e,t,n,!0)},i)},detachDelegate:function(e,t,n,r){var i=t[v];i&&(i.detach(),t[v]=null),P(e)},detach:function(e,t,n){var r=t[h];r&&(r.detach(),t[h]=null),P(e)},processArgs:function(e,t){return A(this,e,t)},_onEnd:function(e,t,n,r,i){i&&(t=e[x]);var s=n._extra.standAlone||t.getData(g)||t.getData(m),o=n._extra.preventDefault;s&&(e.changedTouches&&(e.changedTouches.length===1?_(e,e.changedTouches[0]):s=!1),s&&(H(e,o),e.type=a,r.fire(e),t.clearData(m),t.clearData(g)))},PREVENT_DEFAULT:!1})},"3.7.3",{requires:["node-base","event-touch","event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-flick",function(e,t){var n=e.Event._GESTURE_MAP,r={start:n.start,end:n.end,move:n.move},i="start",s="end",o="move",u="ownerDocument",a="minVelocity",f="minDistance",l="preventDefault",c="_fs",h="_fsh",p="_feh",d="_fmh",v="nodeType";e.Event.define("flick",{on:function(e,t,n){var s=e.on(r[i],this._onStart,this,e,t,n);t[h]=s},detach:function(e,t,n){var r=t[h],i=t[p];r&&(r.detach(),t[h]=null),i&&(i.detach(),t[p]=null)},processArgs:function(t){var n=t.length>3?e.merge(t.splice(3,1)[0]):{};return a in n||(n[a]=this.MIN_VELOCITY),f in n||(n[f]=this.MIN_DISTANCE),l in n||(n[l]=this.PREVENT_DEFAULT),n},_onStart:function(t,n,i,a){var f=!0,l,h,m,g=i._extra.preventDefault,y=t;t.touches&&(f=t.touches.length===1,t=t.touches[0]),f&&(g&&(!g.call||g(t))&&y.preventDefault(),t.flick={time:(new Date).getTime()},i[c]=t,l=i[p],m=n.get(v)===9?n:n.get(u),l||(l=m.on(r[s],e.bind(this._onEnd,this),null,n,i,a),i[p]=l),i[d]=m.once(r[o],e.bind(this._onMove,this),null,n,i,a))},_onMove:function(e,t,n,r){var i=n[c];i&&i.flick&&(i.flick.time=(new Date).getTime())},_onEnd:function(e,t,n,r){var i=(new Date).getTime(),s=n[c],o=!!s,u=e,h,p,v,m,g,y,b,w,E=n[d];E&&(E.detach(),delete n[d]),o&&(e.changedTouches&&(e.changedTouches.length===1&&e.touches.length===0?u=e.changedTouches[0]:o=!1),o&&(m=n._extra,v=m[l],v&&(!v.call||v(e))&&e.preventDefault(),h=s.flick.time,i=(new Date).getTime(),p=i-h,g=[u.pageX-s.pageX,u.pageY-s.pageY],m.axis?w=m.axis:w=Math.abs(g[0])>=Math.abs(g[1])?"x":"y",y=g[w==="x"?0:1],b=p!==0?y/p:0,isFinite(b)&&Math.abs(y)>=m[f]&&Math.abs(b)>=m[a]&&(e.type="flick",e.flick={time:p,distance:y,velocity:b,axis:w,start:s},r.fire(e)),n[c]=null))},MIN_VELOCITY:0,MIN_DISTANCE:0,PREVENT_DEFAULT:!1})},"3.7.3",{requires:["node-base","event-touch","event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-valuechange",function(e,t){var n="_valuechange",r="value",i,s={POLL_INTERVAL:50,TIMEOUT:1e4,_poll:function(t,r){var i=t._node,o=r.e,u=i&&i.value,a=t._data&&t._data[n],f,l;if(!i||!a){s._stopPolling(t);return}l=a.prevVal,u!==l&&(a.prevVal=u,f={_event:o,currentTarget:o&&o.currentTarget||t,newVal:u,prevVal:l,target:o&&o.target||t},e.Object.each(a.notifiers,function(e){e.fire(f)}),s._refreshTimeout(t))},_refreshTimeout:function(e,t){if(!e._node)return;var r=e.getData(n);s._stopTimeout(e),r.timeout=setTimeout(function(){s._stopPolling(e,t)},s.TIMEOUT)},_startPolling:function(t,i,o){if(!t.test("input,textarea"))return;var u=t.getData(n);u||(u={prevVal:t.get(r)},t.setData(n,u)),u.notifiers||(u.notifiers={});if(u.interval){if(!o.force){u.notifiers[e.stamp(i)]=i;return}s._stopPolling(t,i)}u.notifiers[e.stamp(i)]=i,u.interval=setInterval(function(){s._poll(t,u,o)},s.POLL_INTERVAL),s._refreshTimeout(t,i)},_stopPolling:function(t,r){if(!t._node)return;var i=t.getData(n)||{};clearInterval(i.interval),delete i.interval,s._stopTimeout(t),r?i.notifiers&&delete i.notifiers[e.stamp(r)]:i.notifiers={}},_stopTimeout:function(e){var t=e.getData(n)||{};clearTimeout(t.timeout),delete t.timeout},_onBlur:function(e,t){s._stopPolling(e.currentTarget,t)},_onFocus:function(e,t){var i=e.currentTarget,o=i.getData(n);o||(o={},i.setData(n,o)),o.prevVal=i.get(r),s._startPolling(i,t,{e:e})},_onKeyDown:function(e,t){s._startPolling(e.currentTarget,t,{e:e})},_onKeyUp:function(e,t){(e.charCode===229||e.charCode===197)&&s._startPolling(e.currentTarget,t,{e:e,force:!0})},_onMouseDown:function(e,t){s._startPolling(e.currentTarget,t,{e:e})},_onSubscribe:function(t,i,o,u){var a,f,l;f={blur:s._onBlur,focus:s._onFocus,keydown:s._onKeyDown,keyup:s._onKeyUp,mousedown:s._onMouseDown},a=o._valuechange={};if(u)a.delegated=!0,a.getNodes=function(){return t.all("input,textarea").filter(u)},a.getNodes().each(function(e){e.getData(n)||e.setData(n,{prevVal:e.get(r)})}),o._handles=e.delegate(f,t,u,null,o);else{if(!t.test("input,textarea"))return;t.getData(n)||t.setData(n,{prevVal:t.get(r)}),o._handles=t.on(f,null,null,o)}},_onUnsubscribe:function(e,t,n){var r=n._valuechange;n._handles&&n._handles.detach(),r.delegated?r.getNodes().each(function(e){s._stopPolling(e,n)}):s._stopPolling(e,n)}};i={detach:s._onUnsubscribe,on:s._onSubscribe,delegate:s._onSubscribe,detachDelegate:s._onUnsubscribe,publishConfig:{emitFacade:!0}},e.Event.define("valuechange",i),e.Event.define("valueChange",i),e.ValueChange=s},"3.7.3",{requires:["event-focus","event-synthetic"]});
/*
YUI 3.7.3 (build 5687)
Copyright 2012 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/
YUI.add("event-tap",function(e,t){function l(t,n,r,i){n=r?n:[n.START,n.MOVE,n.END,n.CANCEL],e.Array.each(n,function(e,n,r){var i=t[e];i&&(i.detach(),t[e]=null)})}var n=e.config.doc,r=!!n&&!!n.createTouch,i=r?"touchstart":"mousedown",s=r?"touchmove":"mousemove",o=r?"touchend":"mouseup",u=r?"touchcancel":"mousecancel",a="tap",f={START:"Y_TAP_ON_START_HANDLE",MOVE:"Y_TAP_ON_MOVE_HANDLE",END:"Y_TAP_ON_END_HANDLE",CANCEL:"Y_TAP_ON_CANCEL_HANDLE"};e.Event.define(a,{on:function(e,t,n){t[f.START]=e.on(i,this.touchStart,this,e,t,n)},detach:function(e,t,n){l(t,f)},delegate:function(e,t,n,r){t[f.START]=e.delegate(i,function(r){this.touchStart(r,e,t,n,!0)},r,this)},detachDelegate:function(e,t,n){l(t,f)},touchStart:function(e,t,n,i,a){var l={canceled:!1};if(e.button&&e.button===3)return;if(e.touches&&e.touches.length!==1)return;l.node=a?e.currentTarget:t,r&&e.touches?l.startXY=[e.touches[0].pageX,e.touches[0].pageY]:l.startXY=[e.pageX,e.pageY],n[f.MOVE]=t.once(s,this.touchMove,this,t,n,i,a,l),n[f.END]=t.once(o,this.touchEnd,this,t,n,i,a,l),n[f.CANCEL]=t.once(u,this.touchMove,this,t,n,i,a,l)},touchMove:function(e,t,n,r,i,s){l(n,[f.MOVE,f.END,f.CANCEL],!0,s),s.cancelled=!0},touchEnd:function(e,t,n,i,s,o){var u=o.startXY,c,h;r&&e.changedTouches?(c=[e.changedTouches[0].pageX,e.changedTouches[0].pageY],h=[e.changedTouches[0].clientX,e.changedTouches[0].clientY]):(c=[e.pageX,e.pageY],h=[e.clientX,e.clientY]),l(n,[f.MOVE,f.END,f.CANCEL],!0,o),Math.abs(c[0]-u[0])===0&&Math.abs(c[1]-u[1])===0&&(e.type=a,e.pageX=c[0],e.pageY=c[1],e.clientX=h[0],e.clientY=h[1],e.currentTarget=o.node,i.fire(e))}})},"3.7.3",{requires:["node-base","event-base","event-touch","event-synthetic"]});
YUI.add('moodle-core-formchangechecker',
    function(Y) {
        // The CSS selectors we use
        var CSS = {
        };

        var FORMCHANGECHECKERNAME = 'core-formchangechecker';

        var FORMCHANGECHECKER = function() {
            FORMCHANGECHECKER.superclass.constructor.apply(this, arguments);
        }

        Y.extend(FORMCHANGECHECKER, Y.Base, {

                // The delegated listeners we need to detach after the initial value has been stored once
                initialvaluelisteners : [],

                /**
                 * Initialize the module
                 */
                initializer : function(config) {
                    var formid = 'form#' + this.get('formid');

                    // Add change events to the form elements
                    var currentform = Y.one(formid);
                    currentform.delegate('change', M.core_formchangechecker.set_form_changed, 'input', this);
                    currentform.delegate('change', M.core_formchangechecker.set_form_changed, 'textarea', this);
                    currentform.delegate('change', M.core_formchangechecker.set_form_changed, 'select', this);

                    // Add a focus event to check for changes which are made without triggering a change event
                    this.initialvaluelisteners.push(currentform.delegate('focus', this.store_initial_value, 'input', this));
                    this.initialvaluelisteners.push(currentform.delegate('focus', this.store_initial_value, 'textarea', this));
                    this.initialvaluelisteners.push(currentform.delegate('focus', this.store_initial_value, 'select', this));

                    // We need any submit buttons on the form to set the submitted flag
                    Y.one(formid).on('submit', M.core_formchangechecker.set_form_submitted, this);

                    // YUI doesn't support onbeforeunload properly so we must use the DOM to set the onbeforeunload. As
                    // a result, the has_changed must stay in the DOM too
                    window.onbeforeunload = M.core_formchangechecker.report_form_dirty_state;
                },

                /**
                 * Store the initial value of the currently focussed element
                 *
                 * If an element has been focussed and changed but not yet blurred, the on change
                 * event won't be fired. We need to store it's initial value to compare it in the
                 * get_form_dirty_state function later.
                 */
                store_initial_value : function(e) {
                    if (e.target.hasClass('ignoredirty')) {
                        // Don't warn on elements with the ignoredirty class
                        return;
                    }
                    if (M.core_formchangechecker.get_form_dirty_state()) {
                        // Clear the store_initial_value listeners as the form is already dirty so
                        // we no longer need to call this function
                        var formid = 'form#' + this.get('formid');

                        // Detach all listen events to prevent duplicate initial value setting
                        var thisevent;
                        while (thisevent = this.initialvaluelisteners.shift()) {
                            thisevent.detach();
                        }

                        return;
                    }

                    // Make a note of the current element so that it can be interrogated and
                    // compared in the get_form_dirty_state function
                    M.core_formchangechecker.stateinformation.focused_element = {
                        element : e.target,
                        initial_value : e.target.get('value')
                    }
                }
            },
            {
                NAME : FORMCHANGECHECKERNAME,
                ATTRS : {
                    formid : {
                        'value' : ''
                    }
                }
            }
        );

        M.core_formchangechecker = M.core_formchangechecker || {};

        // We might have multiple instances of the form change protector
        M.core_formchangechecker.instances = M.core_formchangechecker.instances || [];
        M.core_formchangechecker.init = function(config) {
            var formchangechecker = new FORMCHANGECHECKER(config);
            M.core_formchangechecker.instances.push(formchangechecker);
            return formchangechecker;
        }

        // Store state information
        M.core_formchangechecker.stateinformation = [];

        /**
         * Set the form changed state to true
         */
        M.core_formchangechecker.set_form_changed = function(e) {
            if (e && e.target && e.target.hasClass('ignoredirty')) {
                // Don't warn on elements with the ignoredirty class
                return;
            }
            M.core_formchangechecker.stateinformation.formchanged = 1;

            // Once the form has been marked as dirty, we no longer need to keep track of form elements
            // which haven't yet blurred
            delete M.core_formchangechecker.stateinformation.focused_element;
        }

        /**
         * Set the form submitted state to true
         */
        M.core_formchangechecker.set_form_submitted = function() {
            M.core_formchangechecker.stateinformation.formsubmitted = 1;
        }

        /**
         * Attempt to determine whether the form has been modified in any way and
         * is thus 'dirty'
         *
         * @return Integer 1 is the form is dirty; 0 if not
         */
        M.core_formchangechecker.get_form_dirty_state = function() {
            var state = M.core_formchangechecker.stateinformation;

            // If the form was submitted, then return a non-dirty state
            if (state.formsubmitted) {
                return 0;
            }

            // If any fields have been marked dirty, return a dirty state
            if (state.formchanged) {
                return 1;
            }

            // If a field has been focused and changed, but still has focus then the browser won't fire the
            // onChange event. We check for this eventuality here
            if (state.focused_element) {
                if (state.focused_element.element.get('value') != state.focused_element.initial_value) {
                    return 1;
                }
            }

            // Handle TinyMCE editor instances
            // We can't add a listener in the initializer as the editors may not have been created by that point
            // so we do so here instead
            if (typeof tinyMCE != 'undefined') {
                for (var editor in tinyMCE.editors) {
                    if (tinyMCE.editors[editor].isDirty()) {
                        return 1;
                    }
                }
            }

            // If we reached here, then the form hasn't met any of the dirty conditions
            return 0;
        };

        /**
         * Return a suitable message if changes have been made to a form
         */
        M.core_formchangechecker.report_form_dirty_state = function(e) {
            if (!M.core_formchangechecker.get_form_dirty_state()) {
                // the form is not dirty, so don't display any message
                return;
            }

            // This is the error message that we'll show to browsers which support it
            var warningmessage = M.util.get_string('changesmadereallygoaway', 'moodle');

            // Most browsers are happy with the returnValue being set on the event
            // But some browsers do not consistently pass the event
            if (e) {
                e.returnValue = warningmessage;
            }

            // But some require it to be returned instead
            return warningmessage;
        };
    },
    '@VERSION@', {
        requires : ['base', 'event-focus']
    }
);
