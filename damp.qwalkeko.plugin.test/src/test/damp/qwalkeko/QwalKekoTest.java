package test.damp.qwalkeko;

import static org.junit.Assert.assertEquals;

import java.io.File;

import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import qwalkeko.Activator;
import test.damp.EkekoTestHelper;

public class QwalKekoTest {
	
	private static Bundle myBundle;
	private static Bundle ekekoBundle;


	static {
		myBundle = FrameworkUtil.getBundle(QwalKekoTest.class);
		ekekoBundle = FrameworkUtil.getBundle(EkekoTestHelper.class);
	}

		
	
	@BeforeClass
	public static void ensureTestCasesExist() throws Exception {
		//EkekoTestHelper.ensureProjectImported(myBundle, "/resources/", "Ekeko-JDT"); 
		//name of folder under /resources/ containing a .project
	}

	@Test
	public void testPluginID() {
		assertEquals(Activator.PLUGIN_ID, "damp.qwalkeko.plugin");		
	}

	@Test 
	public void testIntegration() {
		EkekoTestHelper.testClojureNamespace(myBundle, "test.damp.qwalkeko.integration");
	}


}
