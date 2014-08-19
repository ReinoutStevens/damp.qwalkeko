package qwalkeko;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;



public class HistoryNature implements IProjectNature{
	
	public static final String NATURE_ID = Activator.PLUGIN_ID + ".historyNature";

	private IProject project;

	public static void toggleNature(IProject project){
		try {
			damp.util.Natures.toggleNature(project, NATURE_ID);
		} catch (CoreException e) {
			e.printStackTrace();
		}
				
	}

	@Override
	public void configure() throws CoreException {
		System.out.println("History project nature added to: " + project.getName());
		
	}

	@Override
	public void deconfigure() throws CoreException {
		System.out.println("History project nature removed from: " + project.getName());
		
	}

	@Override
	public IProject getProject() {
		return project;
	}

	@Override
	public void setProject(IProject project) {
		this.project = project;
	}
	
	
}
