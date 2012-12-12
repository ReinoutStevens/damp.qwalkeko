package qwalkeko;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;


import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRemoteException;
import org.eclipse.jgit.api.errors.TransportException;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.storage.file.FileRepository;

public class MetaProject {
	public static final String xmlName = "project.xml";
	
	
	private Git sourceRepository;
	private Git metaRepository;
	


	private String name;
	private URI uri;
	
	private Map<String, MetaVersion> versions;
	private Collection<MetaVersion> roots;
	private IProject eclipseProject;
	
		
	public MetaProject(String name, URI url){
		this.name = name;
		this.uri = url;
		this.versions = new TreeMap<String, MetaVersion>();
		this.roots = new ArrayList<MetaVersion>();
		this.eclipseProject = null;
	}
	
	public String getName() {
		return name;
	}


	public void setName(String name) {
		this.name = name;
	}



	public URI getURI() {
		return uri;
	}



	public void setURI(URI uri) {
		this.uri = uri;
	}
	
	public void setSourceRepository() throws IOException {
		File file = new File(uri.toString());
		Repository rep = new FileRepository(file);
		this.sourceRepository = new Git(rep);
	}
	
	public Git getSourceRepository() {
		assert(sourceRepository != null);
		return sourceRepository;
	}
	

	public Git getMetaRepository() {
		assert(metaRepository != null);
		return metaRepository;
	}
	
	public void initialize() throws CoreException{
		try{
		this.createEclipseProjects();
		for(MetaVersion v : versions.values()){
			v.initialize(this);
		}
		this.findAndSetRoots();
		this.setSourceRepository();
		this.createMetaRepository();
		} catch(Exception e){
			IStatus status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e);
			throw new CoreException(status);
		}
	}
	
	
	private void findAndSetRoots(){
		for(MetaVersion v : versions.values()){
			if(v.isRoot()){
				roots.add(v);
			}
		}
	}
	
	public Collection<MetaVersion> getVersions(){
		return versions.values();
	}
	
	public Collection<MetaVersion> getRoots(){
		return roots;
	}
	
	public MetaVersion addVersion(MetaVersion version){
		return versions.put(version.getRevisionNumber(), version);
	}
	
	public void addVersions(Collection<MetaVersion> versions){
		for(MetaVersion v : versions){
			this.versions.put(v.getRevisionNumber(), v);
		}
	}
	
	public void delete(IProgressMonitor monitor) throws CoreException{
		for(MetaVersion v : versions.values()){
			v.delete(monitor);
		}
		if(eclipseProject != null){
			eclipseProject.delete(true, monitor);
		}
	}
	
	public MetaVersion findVersion(String sha){
		return versions.get(sha);
	}
	
	private void createEclipseProjects(){
		eclipseProject = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
		//lets ignore this one...
		try{
			if(!eclipseProject.exists()){
				eclipseProject.create(null);
			}
			
		} catch(CoreException e){
			e.printStackTrace();
		}
	}
	
	private void createMetaRepository() throws InvalidRemoteException, TransportException, GitAPIException, IOException{
		Repository repo = getSourceRepository().getRepository();
		File target = new File(eclipseProject.getLocation().toFile(), repositoryDir());
		if(!target.exists()){ //if it exists we assume it has been cloned before
			Git.cloneRepository().setDirectory(target).setURI(uri.toString()).call();
		}
		Repository metaRepo = new FileRepository(target);
		metaRepository = new Git(metaRepo);
	}
	
	private String repositoryDir(){
		return "repository";
	}
}
