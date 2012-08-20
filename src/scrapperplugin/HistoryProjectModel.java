package scrapperplugin;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.xml.sax.SAXException;

import scrapperplugin.parser.ScrapperHandler;

import damp.ekeko.EkekoNature;
import damp.ekeko.ProjectModel;

public class HistoryProjectModel extends ProjectModel {

	private MetaProject metaProject;

	public HistoryProjectModel(IProject p) {
		super(p);
	}

	public void populate(IProgressMonitor monitor) throws CoreException {
		super.populate(monitor);
		IFile ifile = getProject().getFile(MetaProject.xmlName);
		File file = new File(ifile.getLocationURI());
		if (!file.exists()) {
			IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
					"missing project file " + MetaProject.xmlName);
			throw new CoreException(status);
		}
		try {
			ScrapperHandler handler = ScrapperHandler.parseFile(file);
			metaProject = handler.getMetaProject();
			if (metaProject == null) {
				// something went wrong
				IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
						"something went wrong parsing the file");
				throw new CoreException(status);
			}
		} catch (IOException e) {
			IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
					"something went wrong parsing the file", e);
			throw new CoreException(status);
		} catch (SAXException e) {
			IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
					"something went wrong parsing the file", e);
			throw new CoreException(status);
		}
		System.out.println("Populated history model for project: "
				+ getProject().getName());
		addEkekoNatures();
		System.out.println("Added ekekko natures to all versions");
	}

	public void clean() {
		super.clean();
		this.metaProject = null;
	}

	public MetaProject getMetaProject() {
		return metaProject;
	}
	
	private void addEkekoNatures() throws CoreException{
		for(MetaVersion v : metaProject.getVersions()){
			IProject project = v.getEclipseProject();
			if(!project.isOpen()){
				project.open(null);
			}
			damp.util.Natures.addNature(project, EkekoNature.NATURE_ID);
		}
	}

}
