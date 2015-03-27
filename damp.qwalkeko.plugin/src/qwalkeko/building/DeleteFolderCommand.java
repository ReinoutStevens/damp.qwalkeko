package qwalkeko.building;

import java.io.File;

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
		IFolder ifolder = project.getFolder(folderName);
		if(ifolder != null){
			ifolder.delete(false, null);
		}
	}

	public void deleteFolder(File folder) {
	    File[] files = folder.listFiles();
	    if(files!=null) {
	        for(File f: files) {
	            if(f.isDirectory()) {
	                deleteFolder(f);
	            } else {
	                f.delete();
	            }
	        }
	    }
	    folder.delete();
	}
}
