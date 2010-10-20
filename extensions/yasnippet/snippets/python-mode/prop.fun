# contributor: Mads D. Kristensen <madsdk@gmail.com>
# name: prop
# desc: property with fget/fset/fdel
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
def ${1:foo}():
   doc = """${2:Doc string}"""
   def fget(self):
       return self._$1
   def fset(self, value):
       self._$1 = value
   def fdel(self):
       del self._$1
   return locals()
$1 = property(**$1())

$0
