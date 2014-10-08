package qwalkeko;



import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;

public class ImportRepositoryHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		
		DirectoryDialog dialog = new DirectoryDialog(Display.getCurrent().getActiveShell());
		dialog.setText("Select .git folder");
		String path = dialog.open();
		if(path != null){
			File file = new File(path);
			if(!(file.exists() && file.isDirectory() && file.getName().equals(".git"))){
				throw new ExecutionException("No git folder selected");
			}
			String projectName = file.getParentFile().getName();
			IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
			IProject project = root.getProject(projectName);
			if(!project.exists()){
				try {
					project.create(null);
				} catch (CoreException e) {
					e.printStackTrace();
					throw new ExecutionException(e.getMessage());
				}
				try {
					Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);
					assert(bundle != null);
					IPath jarPath = new Path("lib/git-scrapper-1.0.1.jar");
					URL jarURL = FileLocator.find(bundle, jarPath, null);
					String jarString = FileLocator.getBundleFile(bundle).getAbsolutePath() + jarURL.toURI().getPath();
					String target = project.getFile("project.xml").getLocation().toString();
					ProcessBuilder pb = new ProcessBuilder("java","-jar", jarString, path, target);
					Process proc = pb.start();
					proc.waitFor();
					project.open(null);
					damp.util.Natures.addNature(project, HistoryNature.NATURE_ID);
				} catch (IOException e) {
					throw new ExecutionException(e.getMessage());
				} catch (InterruptedException e) {
					throw new ExecutionException(e.getMessage());
				} catch (URISyntaxException e) {
					throw new ExecutionException(e.getMessage());
				} catch (CoreException e) {
					throw new ExecutionException(e.getMessage());
				}
			}
			System.out.println("Successfully imported project " + projectName);
			System.out.println("Do not forget to include it to the Ekeko projects");
		}
		return null;
	}

}
