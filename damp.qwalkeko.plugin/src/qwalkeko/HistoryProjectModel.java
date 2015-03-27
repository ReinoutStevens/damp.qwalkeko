package qwalkeko;

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

import qwalkeko.building.ProjectBuilder;
import qwalkeko.parser.ProjectBuilderHandler;
import qwalkeko.parser.ScrapperHandler;
import damp.ekeko.EkekoNature;
import damp.ekeko.ProjectModel;

public class HistoryProjectModel extends ProjectModel {

	private MetaProduct metaProduct;

	public HistoryProjectModel(IProject p) {
		super(p);
	}

	public void populate(IProgressMonitor monitor) throws CoreException {
		super.populate(monitor);
		buildMetaProduct();
		System.out.println("Populated history model for project: "
				+ getProject().getName());
		
	}

	public void clean() {
		super.clean();
		this.metaProduct = null;
	}

	public MetaProduct getMetaProduct() {
		return metaProduct;
	}
	
	
	private void buildMetaProduct() throws CoreException{
		IFile ifile = getProject().getFile(MetaProduct.xmlName);
		File file = new File(ifile.getLocationURI());
		if (!file.exists()) {
			IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
					"missing project file " + MetaProduct.xmlName);
			throw new CoreException(status);
		}
		try {
			ScrapperHandler handler = ScrapperHandler.parseFile(file);
			metaProduct = handler.getMetaProduct();
			if (metaProduct == null) {
				// something went wrong
				IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
						"something went wrong parsing the file");
				throw new CoreException(status);
			}
			ProjectBuilder builder = buildProjectBuilder();
			for(MetaProject p : metaProduct.getMetaProjects()){
				p.setBuilder(builder);
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
	}

	private ProjectBuilder buildProjectBuilder() throws CoreException{
		IFile ifile = getProject().getFile(ProjectBuilder.xmlName);
		File file = new File(ifile.getLocationURI());
		ProjectBuilder builder = null;
		if(file.exists()){
			//assume we dont have to do anything special if file doesnt exist
				ProjectBuilderHandler handler;
				try {
					handler = ProjectBuilderHandler.parseFile(file);
					builder = handler.getProjectBuilder();
				} catch (IOException e) {
					IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
							"something went wrong parsing the buildfile", e);
					throw new CoreException(status);
				} catch (SAXException e) {
					IStatus status = new Status(Status.ERROR, "ScrapperPlugin",
							"something went wrong parsing the buildfile", e);
					throw new CoreException(status);
				}
				
		} else {
			builder = new ProjectBuilder();
		}
		assert(builder != null);
		return builder;
	}
}
