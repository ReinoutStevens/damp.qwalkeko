package qwalkeko;






public class ChangedFileInfo {
	public enum Status {
	    ADD, DELETE, EDIT
	}
	
	private String fileName;
	private Status status;

	public ChangedFileInfo(String fileName, Status status){
		this.fileName = fileName;
		this.status = status;
	}
	
	public String getFileName() {
		return fileName;
	}
	public Status getStatus() {
		return status;
	}
	
	public boolean wasChanged(){
		return status == Status.ADD || status == Status.EDIT;
	}
	
	public boolean wasEdited(){
		return status == Status.EDIT;
	}
}
