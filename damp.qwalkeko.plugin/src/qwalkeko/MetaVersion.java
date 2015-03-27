package qwalkeko;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.TreeSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import damp.ekeko.EkekoNature;

//look at ProjectNature
public class MetaVersion {

	private final String revisionNumber; //used as key in some places

	private Collection<String> successorRevisions;
	private Collection<String> predecessorRevisions;
	private Collection<ChangedFileInfo> changedFiles;
	
	
	private String commitMessage;
	private String author;
	private Calendar time;
	
	private MetaProject metaProject;
	private IProject eclipseProject;
	
	public static String retrieveRevisionNoFromProject(IProject project){
		String name = project.getName();
		int idx = name.lastIndexOf("-");
		return name.substring(idx + 1);
	}
	
	public MetaVersion(String revNo, String commitMessage, String author, Calendar time){
		this.revisionNumber = revNo;
		this.successorRevisions = new TreeSet<String>();
		this.predecessorRevisions = new TreeSet<String>();
		this.commitMessage = commitMessage;
		this.author = author;
		this.time = time;
		this.metaProject = null;
		this.changedFiles = new ArrayList<ChangedFileInfo>();
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
	
	public void initialize(MetaProject project){
		this.metaProject = project;		
	}
	
		
	
	public IProject getEclipseProject(){
		return eclipseProject;
	}
	
	public void delete(IProgressMonitor monitor) throws CoreException{
		if(eclipseProject != null){
			eclipseProject.delete(true, monitor);
		}
		eclipseProject = null;
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
	
	
	public Collection<ChangedFileInfo> getChangedFileInfos(){
		return changedFiles;
	}
	
	public void addChangedFiles(Collection<ChangedFileInfo> changed){
		this.changedFiles.addAll(changed);
	}
	
	public void openAndCheckoutIfNeeded() throws CoreException{
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		try{
			if(!targetLocation.exists()){
				File sourceLocation = metaProject.getMetaRepository();
				doCloneOperation(sourceLocation, targetLocation);
				doCheckoutOperation();
				//eclipse does silly things when a .git is present, especially when you delete that folder later
				deleteGitFolder();
			} 
		} catch(Exception e){
			IStatus status = new Status(Status.ERROR, Activator.PLUGIN_ID, e.getMessage(), e);
			throw new CoreException(status);
		}
		if(eclipseProject == null || !eclipseProject.exists()){
			createEclipseMetaProject();
		}
	}
	
	public void closeAndDeleteIfNeeded() throws CoreException{
		if(eclipseProject != null){
			damp.util.Natures.removeNature(eclipseProject, EkekoNature.NATURE_ID);
			eclipseProject.delete(true, false, null);
			eclipseProject = null;
		}
	}
	
	private void doCloneOperation(File sourceLocation, File targetLocation) throws IOException, InterruptedException{
		assert(sourceLocation.exists());
		assert(!targetLocation.exists());
		String[] cmd = { GitCommands.gitCommand(), "clone", "--shared", sourceLocation.getAbsolutePath(), targetLocation.getAbsolutePath() };
		Process proc = Runtime.getRuntime().exec(cmd);
		if(proc.waitFor() != 0){
			assert(false);
		}
	}
	
	
	private void doCheckoutOperation() throws IOException, InterruptedException{
		/*(Repository repo = versionRepository.getRepository();
		ObjectId head = repo.resolve(Constants.HEAD);
		Iterable<RevCommit> commits = versionRepository.log().add(head).call();
		RevCommit commit = null;
		for(RevCommit c : commits){
			if(c.getId().getName().equals(this.getRevisionNumber())){
				commit = c;
				break;
			}
		}
		versionRepository.checkout().setStartPoint(commit).setAllPaths(true).call();
		*/
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		String[] cmd = { GitCommands.gitCommand(), "checkout", getRevisionNumber() };
		Process proc = Runtime.getRuntime().exec(cmd, null, targetLocation);
		if(proc.waitFor() != 0){
			assert(false);
		}
	}
	
	private void deleteGitFolder() {
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		File gitLocation = new File(targetLocation, ".git");
		deleteDir(gitLocation);
	}
	
	private String getVersionRepositoryLocation(){
		return metaProject.getName() + "-" + this.getRevisionNumber();
	}
	
	private void createEclipseMetaProject(){
		String loc = getVersionRepositoryLocation();
		eclipseProject = ResourcesPlugin.getWorkspace().getRoot().getProject(loc);
		//we get errors here, dont know why...
		try{
			if(!eclipseProject.exists()){
				eclipseProject.create(null);
			}
			if(!eclipseProject.isOpen()){
				eclipseProject.open(null);
			}
			metaProject.getBuilder().build(eclipseProject);
			//always add ekeko nature last as the model starts building once the nature is added
			//some natures modify how ekeko builds its model
			String[] natures = new String[]{ QwalkekoNature.NATURE_ID, EkekoNature.NATURE_ID };
			for (int i = 0; i < natures.length; i++) {
				String nature = natures[i];
				if(!eclipseProject.hasNature(nature)){
					damp.util.Natures.addNature(eclipseProject, nature);
				}
			}
			try {
				Job.getJobManager().join(ResourcesPlugin.FAMILY_AUTO_BUILD, null);
			} catch (OperationCanceledException | InterruptedException e) {
				e.printStackTrace();
				return;
			}
		} catch(CoreException e){
			e.printStackTrace();
		}
	}
	
	private boolean deleteDir(File dir) { 
	  if (dir.isDirectory()) { 
		  String[] children = dir.list(); 
		  for (int i=0; i<children.length; i++) { 
			  boolean success = deleteDir(new File(dir, children[i])); 
			  if (!success) {  
				  return false; 
			  } 
		  }
	  }	
	  return dir.delete(); 
	}
	
}
