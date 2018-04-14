# /* vim: set tabstop=2 softtabstop=2 shiftwidth=2 expandtab : */ 

class d:
  def __init__(i, tag, txt, nl="", kl=None):
    i.tag, i.nl, i.kl, i.txt = tag, nl, kl, txt
  def __repr__(i):
    s=""
    if isinstance(i.txt,(list,tuple)):
      s = ''.join([str(x) for x in i.txt])
    else:
      s = str(i.txt)
    kl = " class=\"%s\"" % i.kl if i.kl else ""
    return "%s<%s%s>%s</%s>" % (
              i.nl,i.tag,kl,s,i.tag)

def dnl(tag,txt,kl=None): return d(tag, txt,kl=kl,nl="\n")

# n different licences
# sidenav
# top nav
# news
# all the following should be sub-classed

class Page:
  def page(i,t,x): return dnl("html", [i.head(t), i.body( i.div(x, "wrapper"))])
  def body(i,x, kl=None) : return dnl( "body", x,                  kl=kl)     
  def head(i,t, kl=None) : return dnl( "head", i.title(t),         kl=kl)
  def title(i,x,kl=None) : return dnl( "title",x,                  kl=kl)     
  def div(i,x,  kl=None) : return dnl( "div",  x,                  kl=kl)     
  def ul(i,x,   kl=None) : return d(   "ul",   x,                  kl=kl)     
  def i(i,x,    kl=None) : return d(   "em",   x,                  kl=kl)     
  def b(i,x,    kl=None) : return d(   "b" ,   x,                  kl=kl)     
  def p(i,x,    kl=None) : return dnl( "p" ,   x,                  kl=kl)     
  def li(i,x,   kl=None) : return dnl( "li",   x,                  kl=kl)     
  def ol(i,*l,  kl=None) : return dnl( "ol",   [i.li(y) for y in l], kl=kl)
  def ul(i,*l,  kl=None) : return dnl( "ul",   [i.li(y) for y in l], kl=kl)
  
  def uls(i,*l, kl=None, odd="li0", even="li1"): 
    return i.ls(*l,what="ul",kl=None,odd=odd,even=even)
  def ols(i,*l, kl=None, odd="li0", even="li1"): 
    return i.ls(*l,what="ol",kl=None,odd=odd,even=even)
  def ls(i,*l,  what="ul", kl=None, odd="li0", even="li1"):
    oddp=[False]
    def show(x):
      oddp[0] = not oddp[0]
      return dnl("li",  x, kl = odd if oddp[0]  else even)
    return dnl( what, [show(y) for y in l],kl=kl)     

p=Page()
print(p.page("love",p.uls("asdas",["sdasas", p.b("bols")], "dadas","apple", 
          "banana","ws","white",odd="odd", even="even")))

