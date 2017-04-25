module Plugin


import reff::Syntax;


import ParseTree;
import util::IDE;
import Message;
import IO;

private str REFF_LANG ="Reff";

anno rel[loc, loc, str] Tree@hyperlinks;

public void setupReff() {
  
  registerLanguage(REFF_LANG, "reff", Tree(str src, loc l) {
    return parse(#start[Program], src, l);
  });
  
 
}
