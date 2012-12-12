package qwalkeko;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IProject;

import damp.ekeko.IProjectModel;
import damp.ekeko.IProjectModelFactory;

public class HistoryProjectModelFactory implements IProjectModelFactory{

	@Override
	public IProjectModel createModel(IProject project) {
		return new HistoryProjectModel(project);
	}
	
	public Collection<String> applicableNatures(){
		Collection<String> result = new ArrayList<String>(1);
		result.add(HistoryNature.NATURE_ID);
		return result;
	}

	@Override
	public Collection<IProjectModelFactory> conflictingFactories(IProject p,
			Collection<IProjectModelFactory> applicableFactories) {
		return new ArrayList<IProjectModelFactory>();
	}

}
