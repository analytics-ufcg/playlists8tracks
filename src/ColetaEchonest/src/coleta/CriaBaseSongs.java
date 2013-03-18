package coleta;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import com.echonest.api.v4.ArtistParams;
import com.echonest.api.v4.EchoNestAPI;
import com.echonest.api.v4.EchoNestException;
import com.echonest.api.v4.Params;
import com.echonest.api.v4.Segment;
import com.echonest.api.v4.Song;

public class CriaBaseSongs {
	private EchoNestAPI en, en1, en2, en3;
	//	final static Charset ENCODING = StandardCharsets.ISO_8859_1;
	final static String PASTA_TIMBRE = "timbres/";
	int indexAtual;

	public CriaBaseSongs() {
		final String[] APIKeys = { "WFY3BMON2KYKZG19U", "ILKZUHGM3CMJKJ9OI", "8DBZK8RH9HZBGUERB" };
		en1 = new EchoNestAPI(APIKeys[0]);
		en2 = new EchoNestAPI(APIKeys[1]);
		en3 = new EchoNestAPI(APIKeys[2]);
		en = en1; 
		indexAtual = 0;
		en1.setTraceSends(false);
		en1.setTraceRecvs(false);
		en2.setTraceSends(false);
		en2.setTraceRecvs(false);
		en3.setTraceSends(false);
		en3.setTraceRecvs(false);

	}

	private void getProximaAPIKey() {
		if (indexAtual == 0) {
			en = en2;
			indexAtual = 1;
		} else if (indexAtual == 1) {
			en = en3;
			indexAtual = 2;
		} else {
			throw new RuntimeException("No more valid keys!");
		}

	}

	private static Song identificaCancao(List<Song> songs, String artist) {
		for (Song s : songs) {
			if (s.getArtistName().toLowerCase().contains(artist.toLowerCase().trim())) {
				return s;
			}
		}
		return null;

	}

	private String searchSongByTitleAndArtist(String title, String artist, String songID) throws EchoNestException{
		ArtistParams ap = new ArtistParams();
		Params p = new Params();

		ap.addName(artist);
		List<Song> songs;
		p.add("artist", artist);
		p.add("title", title);
		p.add("results", 50);

		songs = en.searchSongs(p);

		Song song = identificaCancao(songs, artist);

		if (song != null) {
			String arquivoTimbre = PASTA_TIMBRE + songID + ".txt";;
			String resultado = String.format(
					"%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%d\t%s",
					song.getSongHotttnesss(),
					song.getTempo(), song.getDanceability(),
					song.getEnergy(), song.getLoudness(),
					song.getDuration(), song.getTimeSignature(),
					arquivoTimbre);

			FileWriter writer;
			try {
				writer = new FileWriter(arquivoTimbre);
				writer.write(getTimbres(song));
				writer.flush();
				writer.close();
				return resultado;
			} catch (IOException e) {
				e.printStackTrace();
				throw new RuntimeException(e);
			}

		}

		return "";
	}

	private static String getTimbres(Song song) throws EchoNestException {
		
		try{
			StringBuilder resultado = new StringBuilder();
			List<Segment> segmentos = song.getAnalysis().getSegments();
			
			if(segmentos == null){
				return resultado.toString();
			}

			for (Segment s : segmentos) {
				resultado.append(Arrays.toString(s.getTimbre()));
				resultado.append('\n');
			}
			
			return resultado.toString().replaceAll("\\[|\\]", "").replaceAll(",\\s", ",");
		}catch(Exception e){
			e.printStackTrace();
			return "";
		}
		
	}

	public static void main(String[] args) throws Exception {

		CriaBaseSongs sse = new CriaBaseSongs();

		String inputFileName = args[0];
		String outputFileName = args[1];
		String errorFileName = args[2];

		Scanner input = new Scanner(new File(inputFileName));
		int lineNumber = 0;

		if(new File("contagem.txt").exists()){
			lineNumber = new Scanner(new File("contagem.txt")).nextInt();
		}

		int currentLine = 0;
		while(currentLine < lineNumber){
			if(!input.hasNextLine()){
				input.close();
				throw new RuntimeException("Error skipping lines from input file!");
			}
			currentLine++;
			System.err.println("Skipped line (" + currentLine + "): " + input.nextLine());
		}

		while (input.hasNextLine()) {
			String newLine = input.nextLine();
			String[] splittedLine = newLine.split("\t");

			for (int i = 0; i < splittedLine.length; i++) {
				splittedLine[i] = splittedLine[i].trim();
			}

			String outputLine = "";

			boolean coletou = false;

			while(!coletou){
				try {
					outputLine = sse.searchSongByTitleAndArtist(
							splittedLine[2], splittedLine[1], splittedLine[3]);
					coletou = true;
				} catch (Exception e) {
					e.printStackTrace();
					sse.getProximaAPIKey();
				}

			}

			if (outputLine.isEmpty()) {
				FileWriter writer = new FileWriter(new File(errorFileName),
						true);
				writer.write(newLine + "\n");
				writer.close();
			} else {
				FileWriter writer = new FileWriter(
						new File(outputFileName), true);
				StringBuilder sb = new StringBuilder();
				sb.append(splittedLine[0] + "\t");
				sb.append(splittedLine[1] + "\t");
				sb.append(splittedLine[2] + "\t");
				sb.append(splittedLine[3] + "\t");
				sb.append(outputLine + "\t");
				sb.append(splittedLine[4] + "\n");

				writer.write(sb.toString());
				writer.close();
			}

			FileWriter writer = new FileWriter(new File("contagem.txt"),
					false);
			writer.write("" + (++currentLine));
			writer.close();
		}

		input.close();
	}

}