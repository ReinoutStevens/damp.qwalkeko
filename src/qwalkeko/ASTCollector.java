package qwalkeko;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.TypeDeclaration;

public class ASTCollector extends ASTVisitor {

	Collection<ASTNode> collected = new ArrayList<ASTNode>();


	Collection<Class<?>> classes = new ArrayList<Class<?>>();
	
	
	public void preVisit(ASTNode node){
		super.preVisit(node);
		for(Class<?> c : classes){
			if(c.isInstance(node)){
				collected.add(node);
			}
		}
	}
	
	
	public void addClass(Class<?> cls){
		classes.add(cls);
	}
	
	
	public Collection<ASTNode> getCollected() {
		return collected;
	}

}
