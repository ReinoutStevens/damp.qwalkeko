package qwalkeko.building;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public class DeleteFolderCommand extends BuildCommand {

	private String folderName;
	
	public DeleteFolderCommand(String folder){
		this.folderName = folder;
	}
	
	@Override
	public void execute(IProject project) throws CoreException {
		IFolder folder = project.getFolder(folderName);
		if(folder != null){
			folder.delete(false, null);
		}
	}

}
