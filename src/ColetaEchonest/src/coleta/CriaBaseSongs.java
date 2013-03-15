package coleta;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Scanner;

import com.echonest.api.v4.Artist;
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
		final String[] APIKeys = { "WFY3BMON2KYKZG19U", "ILKZUHGM3CMJKJ9OI",
				"8DBZK8RH9HZBGUERB" };
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
			en = en1;
			indexAtual = 0;
		}

	}

	private Song identificaCancao(List<Song> songs, String artist) {
		for (Song s : songs) {
			if (s.getArtistName().toLowerCase()
					.contains(artist.toLowerCase().trim())) {
				return s;
			}
		}
		return null;

	}

	private String searchSongByTitleAndArtist(String title, String artist, String songID) {
		ArtistParams ap = new ArtistParams();
		Params p = new Params();

		ap.addName(artist);
		List<Artist> artists;
		List<Song> songs;
		String resultado = "";
		int indiceArtista, indiceTitulo;
		try {
			p.add("artist", artist);
			// System.out.println("id de "+artist+
			// " eh "+artists.get(indiceArtista).getID());
			p.add("title", title);
			p.add("results", 50);
			try {
				songs = en.searchSongs(p);
			} catch (Exception e) {
				System.out.println("QUEBROU SONGS");
				System.out.println("indice atual eh " + indexAtual);
				getProximaAPIKey();
				songs = en.searchSongs(p);
			}

			Song song = identificaCancao(songs, artist);

			// artists = en.searchArtists(ap);
			// indiceArtista = artistaEncontrado(artists, artist);
			// if (s != null) {
			//
			//
			// //songs = artists.get(indiceArtista).getSongs();
			// } else {
			// return "\n";
			// }
			// indiceTitulo = musicaEncontrada(songs, title);
			if (song != null) {
				// Song song = songs.get(indiceTitulo);
				// ANO;ARTISTA;ARTISTAID;TITULO;PAIS;BPM;DANCABILIDADE;ENERGIA;SONORIDADE;DURATION;TIMESIGNATURE;ARQUIVO_TIMBRE
				String arquivoTimbre = PASTA_TIMBRE + songID + ".txt";;
				resultado = String.format(
						"%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%d\t%s",
						song.getSongHotttnesss(),
						song.getTempo(), song.getDanceability(),
						song.getEnergy(), song.getLoudness(),
						song.getDuration(), song.getTimeSignature(),
						arquivoTimbre);

				FileWriter fstream;
				try {
					fstream = new FileWriter(arquivoTimbre);
					BufferedWriter saida = new BufferedWriter(fstream);
					String textoTimbre = getTimbres(song);
					saida.write(textoTimbre);
					saida.close();
				} catch (IOException e) {
					e.printStackTrace();
				}

			} else {
				return "";
			}

		} catch (EchoNestException e) {

			e.printStackTrace();
		}
		return resultado;

	}

	private static String transformaArrayString(double[] d) {
		String result = "";
		for (int i = 0; i < d.length; i++) {
			result += d[i] + ",";
		}
		return result.substring(0, result.length() - 1);
	}

	private String getTimbres(Song song) {
		String resultado = "";
		try {
			List<Segment> segmentos = song.getAnalysis().getSegments();
			for (Segment s : segmentos) {
				resultado += transformaArrayString(s.getTimbre()) + "\n";
			}
		} catch (EchoNestException e) {
			System.out.println("Erro ao pegar segmentos");
			e.printStackTrace();
		}
		return resultado;
	}

	public static void main(String[] args) throws FileNotFoundException {
		
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
			input.nextLine();
			currentLine++;
		}
		
		try {
			while (input.hasNextLine()) {
				String newLine = input.nextLine();
				String[] splittedLine = newLine.split("\t");

				for (int i = 0; i < splittedLine.length; i++) {
					splittedLine[i] = splittedLine[i].trim();
				}

				String outputLine = "";
				try {
					outputLine = sse.searchSongByTitleAndArtist(
							splittedLine[2], splittedLine[1], splittedLine[3]);
				} catch (Exception e) {
					sse.getProximaAPIKey();
					outputLine = sse.searchSongByTitleAndArtist(
							splittedLine[2], splittedLine[1], splittedLine[3]);
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
		} catch (Exception e) {
			input.close();
		}
	}

}