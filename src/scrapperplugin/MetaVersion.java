package scrapperplugin;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.TreeSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

//look at ProjectNature
public class MetaVersion {

	private final String revisionNumber; //used as key in some places

	private Collection<String> successorRevisions;
	private Collection<String> predecessorRevisions;
	private String commitMessage;
	private String author;
	private Calendar time;
	private File location;
	
	private MetaProject metaProject;
	private IProject eclipseProject;
	
	public MetaVersion(String revNo, String commitMessage, String author, File location, Calendar time){
		this.revisionNumber = revNo;
		this.successorRevisions = new TreeSet<String>();
		this.predecessorRevisions = new TreeSet<String>();
		this.commitMessage = commitMessage;
		this.author = author;
		this.time = time;
		this.metaProject = null;
		this.location = location;
	}
	

	public String getRevisionNumber() {
		return revisionNumber;
	}
	
		
	public String getCommitMessage() {
		return commitMessage;
	}


	public boolean addSuccessor(String successor){
		return successorRevisions.add(successor);
	}
	
	public boolean addSuccessors(Collection<String> successors){
		return successorRevisions.addAll(successors);
	}
	
	public boolean addPredecessor(String predecessor){
		return predecessorRevisions.add(predecessor);
	}
	
	public boolean addPredecessors(Collection<String> predecessors){
		return predecessorRevisions.addAll(predecessors);
	}
	
	public Collection<MetaVersion> getPredecessors(){
		Collection<MetaVersion> result = new ArrayList<MetaVersion>(predecessorRevisions.size());
		for(String pred : predecessorRevisions){
			MetaVersion version = metaProject.findVersion(pred);
			assert(version != null);
			result.add(version);
		}
		return result;
	}
	
	public Collection<MetaVersion> getSuccessors(){
		Collection<MetaVersion> result = new ArrayList<MetaVersion>(successorRevisions.size());
		for(String pred : successorRevisions){
			MetaVersion version = metaProject.findVersion(pred);
			assert(version != null);
			result.add(version);
		}
		return result;
	}
	
	public boolean isRoot(){
		return this.predecessorRevisions.size() == 0;
	}
	
	public boolean isEndVersion(){
		return this.successorRevisions.size() == 0;
	}
	
	public void finalize(MetaProject project){
		this.metaProject = project;
		createEclipseMetaProject();
		//set predecessors and successors
		
	}
	
		
	
	public IProject getEclipseProject(){
		return eclipseProject;
	}
	
	public void delete(IProgressMonitor monitor) throws CoreException{
		if(eclipseProject != null){
			eclipseProject.delete(true, monitor);
		}
	}
	
	
	
	private void createEclipseMetaProject(){
		String loc = location.toString();
		eclipseProject = ResourcesPlugin.getWorkspace().getRoot().getProject(loc);
		//we get errors here, dont know why...
		try{
			if(!eclipseProject.exists()){
				eclipseProject.create(null);
			}
		} catch(CoreException e){
			e.printStackTrace();
		}
	}


	public Calendar getTime() {
		return time;
	}


	public void setTime(Calendar time) {
		this.time = time;
	}


	public String getAuthor() {
		return author;
	}


	public void setAuthor(String author) {
		this.author = author;
	}
}
