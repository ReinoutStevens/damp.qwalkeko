package qwalkeko;

import java.util.Map;
import java.util.TreeMap;

import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.SimpleName;

/*
 * Matcher that ignores names
 */
public class VariableRenamingMatcher extends ASTMatcher {

	public Map<String,String> renames;
	
	public VariableRenamingMatcher(){
		super();
		this.renames = new TreeMap<String,String>();
	}
	
	
	public boolean match(SimpleName name, Object other){
		if(other instanceof SimpleName){
			SimpleName oname = (SimpleName) other;
			String nameString = name.getIdentifier();
			String onameString = oname.getIdentifier();
		
			if(renames.containsKey(onameString)){
					
				return onameString.equals(renames.get(nameString));
			} else {
				renames.put(nameString, onameString);
				return true;
			}
			
		}
		return false;
	}
}
