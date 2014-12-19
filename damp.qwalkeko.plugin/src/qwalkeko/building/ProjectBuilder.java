package qwalkeko.building;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public class ProjectBuilder {

	public static final String xmlName = "buildCommands.xml";
	private Collection<BuildCommand> commands;
	
	
	
	
	public ProjectBuilder(){
		this.commands = new ArrayList<BuildCommand>();

	}
	
	public void build(IProject project) throws CoreException{
		for(BuildCommand c : commands){
			c.execute(project);
		}
	}
	
	public void addCommand(BuildCommand c){
		commands.add(c);
	}
	
}
