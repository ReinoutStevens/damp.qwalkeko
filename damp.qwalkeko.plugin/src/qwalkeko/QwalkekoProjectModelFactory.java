package qwalkeko;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.JavaCore;

import damp.ekeko.IProjectModel;
import damp.ekeko.IProjectModelFactory;

public class QwalkekoProjectModelFactory implements IProjectModelFactory {

	@Override
	public IProjectModel createModel(IProject project) {
		return new QwalkekoProjectModel(project);
	}

	public Collection<String> applicableNatures(){
		Collection<String> result =  new ArrayList<String>(1);
		result.add(QwalkekoNature.NATURE_ID);
		return result;
	}

	public Collection<IProjectModelFactory> conflictingFactories(IProject p, Collection<IProjectModelFactory> applicableFactories) {
		return Collections.emptySet();
	}
}

