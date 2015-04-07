var jmId_0;
jmId_0 = (function()
          {
            var i;
            i = 0;
            return { 'get': (function()
                             {
                               return i;
                             }), 'increment': (function()
                                               {
                                                 return ++i;
                                               }), 'set': (function(val)
                                                           {
                                                             i = val;
                                                             return null;
                                                           })
                   };
          })();
QUnit.test("module pattern",
(function(jmId_1)
 {
   jmId_1.ok((0 == jmId_0.get()),
   "get ok");
   jmId_0.set(3);
   jmId_1.ok((3 == jmId_0.get()),
   "set ok");
   jmId_0.increment();
   jmId_1.ok((4 == jmId_0.get()),
   "increment ok");
 }));
var jmId_2;
jmId_2 = (function()
          {
            var i;
            i = 0;
            get = (function()
                   {
                     return i;
                   });
            set = (function(val)
                   {
                     i = val;
                     return null;
                   });
            increment = (function()
                         {
                           return ++i;
                         });
            return { 'get': get,
                     'increment': increment,
                     'set': set
                   };
          })();
QUnit.
test("module - functions as local declarations",
(function(jmId_3)
 {
   jmId_3.ok((0 == jmId_2.get()),
   "get ok");
   jmId_2.set(3);
   jmId_3.ok((3 == jmId_2.get()),
   "set ok");
   jmId_2.increment();
   jmId_3.ok((4 == jmId_2.get()),
   "increment ok");
 }));
var jmId_4;
jmId_4 = (function(i)
          {
            return { 'get': (function()
                             {
                               return i;
                             }), 'increment': (function()
                                               {
                                                 return ++i;
                                               }), 'set': (function(val)
                                                           {
                                                             i = val;
                                                             return null;
                                                           })
                   };
          })(0);
QUnit.
test("module - passing in initial values",
(function(jmId_5)
 {
   jmId_5.ok((0 == jmId_4.get()),
   "get ok");
   jmId_4.set(3);
   jmId_5.ok((3 == jmId_4.get()),
   "set ok");
   jmId_4.increment();
   jmId_5.ok((4 == jmId_4.get()),
   "increment ok");
 }));
window.onload = (function()
                 {
                 });