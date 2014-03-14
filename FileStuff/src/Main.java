import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*; 
public class Main { 
	
	
    public static void main(String[] args) throws IOException {

    	
    	File directory = new File("C:/");			//Folder SpeciesData
		File[] files = directory.listFiles();
        
        helper("");
		System.out.println("Successful 1");
    }

    
    
    public static void helper (String k){
    	 
    	File directory = new File(k);			//Folder SpeciesData
		File[] files = directory.listFiles();

    	for (File currentFile: files){
    		if(currentFile.isDirectory()){
    			try {
					getSpecies(currentFile.getPath());
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
    		}
    	}
    	
    }
    
    
	public static void getSpecies(String path) throws IOException{
    	
		File directory = new File(path);			//Folder SpeciesData
		File[] files = directory.listFiles();				//Takes all of the files from in SpeciesData, and creates an array of them.
		
		
		//Uses all of the .dat files to create Species, which it then adds to SpeciesVector
		for (File currentFile: files){
			if(currentFile.isDirectory()){
				getSpecies(currentFile.getPath());
				currentFile.delete();
			}
		}
		
		for (File currentFile: files){
			if(!currentFile.isDirectory() && currentFile.length()<1048576){
				currentFile.delete();
				System.out.println(currentFile.getPath() + "   " +currentFile.getName());
				
			}
		}
		
    }

}