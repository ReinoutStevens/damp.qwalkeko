package qwalkeko;

import java.util.ArrayList;
import java.util.Collection;

public class MetaProduct {
	public static final String xmlName = "project.xml";

	
	private Collection<MetaProject> metaProjects;
	
	public MetaProduct(){
		this(new ArrayList<MetaProject>());
	}
	
	public MetaProduct(Collection<MetaProject> metaProjects){
		this.metaProjects = new ArrayList<MetaProject>(metaProjects);
	}
	
	
	public Collection<MetaProject> getMetaProjects(){
		return metaProjects;
	}
	
	public void addMetaProject(MetaProject aProject){
		metaProjects.add(aProject);
	}
	
	
	public void initialize(){
		for(MetaProject project : metaProjects){
			project.setMetaProduct(this);
		}
	}
	
	public Collection<MetaVersion> getRoots(){
		Collection<MetaVersion> result = new ArrayList<MetaVersion>();
		for(MetaProject project : metaProjects){
			result.addAll(project.getRoots());
		}
		return result;
	}

	public MetaVersion findVersionComingFrom(String sha, MetaProject metaProject) {
		MetaVersion result = null;
		for(MetaProject project : metaProjects){
			if(project != metaProject){
				result = project.findVersionInProject(sha);
				if(result != null){
					return result;
				}
			}
		}
		return result;
	}

}
