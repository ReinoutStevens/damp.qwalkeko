package qwalkeko;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.MethodDeclaration;

public class MethodDeclarationCollector extends ASTVisitor {
	Collection<MethodDeclaration> collected = 
			new ArrayList<MethodDeclaration>();
	
	
	
	 public boolean visit(MethodDeclaration m){
		 collected.add(m);
		 return super.visit(m);
	 }
}
