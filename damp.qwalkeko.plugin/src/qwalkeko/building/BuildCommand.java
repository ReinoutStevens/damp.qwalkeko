package qwalkeko.building;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public abstract class BuildCommand {

	
	public abstract void execute(IProject project) throws CoreException;
}
