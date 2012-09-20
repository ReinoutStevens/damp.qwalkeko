package scrapperplugin;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.TreeSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jgit.api.CheckoutCommand;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.ResetCommand.ResetType;
import org.eclipse.jgit.api.errors.CheckoutConflictException;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRefNameException;
import org.eclipse.jgit.api.errors.JGitInternalException;
import org.eclipse.jgit.api.errors.RefAlreadyExistsException;
import org.eclipse.jgit.api.errors.RefNotFoundException;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.storage.file.FileRepository;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import damp.ekeko.EkekoNature;

//look at ProjectNature
public class MetaVersion {

	private final String revisionNumber; //used as key in some places

	private Collection<String> successorRevisions;
	private Collection<String> predecessorRevisions;
	private String commitMessage;
	private String author;
	private Calendar time;
	
	private MetaProject metaProject;
	private IProject eclipseProject;
	private Git versionRepository;
	
	
	public MetaVersion(String revNo, String commitMessage, String author, Calendar time){
		this.revisionNumber = revNo;
		this.successorRevisions = new TreeSet<String>();
		this.predecessorRevisions = new TreeSet<String>();
		this.commitMessage = commitMessage;
		this.author = author;
		this.time = time;
		this.metaProject = null;
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
	}
	
	public void checkout() throws JGitInternalException, CoreException, CheckoutConflictException, GitAPIException{
		this.checkout(null);
	}
	
	public void checkout(IProgressMonitor monitor) throws JGitInternalException, CoreException, CheckoutConflictException, GitAPIException{
		
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
	
	
	
	public void openAndCheckoutIfNeeded() throws CoreException{
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		try{
			if(!targetLocation.exists()){
				File sourceLocation = metaProject.getMetaRepository().getRepository().getDirectory();
				doCloneOperation(sourceLocation, targetLocation);
				initGitRepository();
				doCheckoutOperation();
			} else {
				//we assume we have checked it out before
				if(versionRepository == null){
					initGitRepository();
				}
			}
		} catch(Exception e){
			IStatus status = new Status(Status.ERROR, Activator.PLUGIN_ID, e.getMessage(), e);
			throw new CoreException(status);
		}
		if(eclipseProject == null){
			createEclipseMetaProject();
		}
	}
	
	public void closeAndDeleteIfNeeded() throws CoreException{
		if(eclipseProject != null){
			eclipseProject.delete(true, false, null);
			eclipseProject = null;
			versionRepository = null;
		}
	}
	
	private void doCloneOperation(File sourceLocation, File targetLocation) throws IOException, InterruptedException{
		assert(sourceLocation.exists());
		assert(!targetLocation.exists());
		String[] cmd = { "git", "clone", "--shared", sourceLocation.getAbsolutePath(), targetLocation.getAbsolutePath() };
		Process proc = Runtime.getRuntime().exec(cmd);
		if(proc.waitFor() != 0){
			assert(false);
			//your mother is dead
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
		String[] cmd = { "git", "checkout", getRevisionNumber() };
		Process proc = Runtime.getRuntime().exec(cmd, null, targetLocation);
		if(proc.waitFor() != 0){
			assert(false);
		}
	}
	
	public Git getVersionRepository(){
		return versionRepository;
	}
	
	private void initGitRepository() throws IOException{
		String location = new File(getVersionRepositoryLocation(), ".git").toString();
		Repository repo = new FileRepository(location);
		versionRepository = new Git(repo);
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
			if(!eclipseProject.hasNature(EkekoNature.NATURE_ID)){
				damp.util.Natures.addNature(eclipseProject, EkekoNature.NATURE_ID);
			}
		} catch(CoreException e){
			e.printStackTrace();
		}
	}
	
}
