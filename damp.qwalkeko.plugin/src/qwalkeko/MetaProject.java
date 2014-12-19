package qwalkeko;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import qwalkeko.building.ProjectBuilder;



public class MetaProject {
	private String name;
	private URI uri;
	
	private Map<String, MetaVersion> versions;
	private Collection<MetaVersion> roots;
	private IProject eclipseProject;
	private MetaProduct metaProduct;
	private ProjectBuilder builder;
		


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
	
	
	public void setMetaProduct(MetaProduct metaProduct) {
		this.metaProduct = metaProduct;
	}
	
	public MetaProduct getMetaProduct(){
		return this.metaProduct;
	}
	
	public ProjectBuilder getBuilder() {
		return builder;
	}

	public void setBuilder(ProjectBuilder builder) {
		this.builder = builder;
	}
	
	public void initialize() throws CoreException{
		try{
			this.createEclipseProjects();
			for(MetaVersion v : versions.values()){
				v.initialize(this);
			}
			this.findAndSetRoots();
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
		MetaVersion localResult = findVersionInProject(sha);
		if(localResult != null){
			return localResult;
		}
		return getMetaProduct().findVersionComingFrom(sha, this);
	}
	
	public MetaVersion findVersionInProject(String sha) {
		return versions.get(sha);
	}
	
	public File getMetaRepository(){
		return new File(eclipseProject.getLocation().toFile(), repositoryDir());
	}
	
	private void createEclipseProjects(){
		eclipseProject = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
		try{
			if(!eclipseProject.exists()){
				eclipseProject.create(null);
			}
			
		} catch(CoreException e){
			e.printStackTrace();
		}
	}
	
	private void createMetaRepository() throws IOException, InterruptedException{
		File target = getMetaRepository();
		if(!target.exists()){ //if it exists we assume it has been cloned before
			String[] cmd = { GitCommands.gitCommand(), "clone", new File(uri.toString()).getParentFile().getAbsolutePath(), target.getAbsolutePath() };
			Process proc = Runtime.getRuntime().exec(cmd);
			if(proc.waitFor() != 0){
				assert(false);
			}
		}
	}
	
	private String repositoryDir(){
		return "repository";
	}

	
}
