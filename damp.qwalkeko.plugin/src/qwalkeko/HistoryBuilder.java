package qwalkeko;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

public class HistoryBuilder extends IncrementalProjectBuilder {

	public static final String BUILDER_ID = "qwalkeko.historyBuilder";
	
	@Override
	protected IProject[] build(int kind, Map<String, String> args,
			IProgressMonitor monitor) throws CoreException {
		//do le nothing
		return null;
	}
	
	protected void startupOnInitialize() {
		// add builder init logic here
		// overriders must call super
		super.startupOnInitialize();
	}


}
