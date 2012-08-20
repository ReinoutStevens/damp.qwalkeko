package scrapperplugin;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class MetaProject {
	public static final String xmlName = "project.xml";
	
	
	private String name;
	private URI url;
	
	private Map<String, MetaVersion> versions;
	private Collection<MetaVersion> roots;
	private IProject eclipseProject;
	
	public MetaProject(String name, URI url){
		this.name = name;
		this.url = url;
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
		return url;
	}



	public void setURI(URI url) {
		this.url = url;
	}

	
	public void finalize(){
		this.createEclipseMetaProject();
		for(MetaVersion v : versions.values()){
			v.finalize(this);
		}
		this.findAndSetRoots();
		
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
	
	private void createEclipseMetaProject(){
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
}
