package coleta;
//package echo.nest;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
//import java.nio.charset.Charset;
//import java.nio.charset.StandardCharsets;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
import java.util.List;

import com.echonest.api.v4.Artist;
import com.echonest.api.v4.ArtistParams;
import com.echonest.api.v4.EchoNestAPI;
import com.echonest.api.v4.EchoNestException;
import com.echonest.api.v4.Params;
import com.echonest.api.v4.Segment;
import com.echonest.api.v4.Song;


public class CriaBaseSongs {
	private EchoNestAPI en, en1, en2, en3;
	final static Charset ENCODING = StandardCharsets.ISO_8859_1;
	final static String PASTA_TIMBRE = "timbres/";
	int indexAtual;

	public CriaBaseSongs() throws EchoNestException {
		final String[] APIKeys = {"WFY3BMON2KYKZG19U", "ILKZUHGM3CMJKJ9OI", "8DBZK8RH9HZBGUERB"};
		en1 = new EchoNestAPI(APIKeys[0]);
		en2 = new EchoNestAPI(APIKeys[1]);
		en3 = new EchoNestAPI(APIKeys[2]);
		en = en1; //começa com a primeira api key
		indexAtual = 0;
		en1.setTraceSends(false);
		en1.setTraceRecvs(false);
		en2.setTraceSends(false);
		en2.setTraceRecvs(false);
		en3.setTraceSends(false);
		en3.setTraceRecvs(false);
		
	}
	
	private void getProximaAPIKey(){
		if (indexAtual == 0){
			en = en2;
			indexAtual = 1;
		}
		else if (indexAtual == 1){
			en = en3;
			indexAtual = 2;
		}
		else{
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

	private String searchSongByTitleAndArtist(String title, String artist) {
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
			} catch(Exception e){
				System.out.println("QUEBROU SONGS");
				System.out.println("indiceatual é "+ indexAtual);
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
				String arquivoTimbre = PASTA_TIMBRE+title.replace("?", "").replace("/", "").replace("\\", "").replace("\"", "").toLowerCase().trim()+"-"+artist.replace("/", "").replace("\\", "").toLowerCase().trim();
				resultado = String.format("%s;%s;%s;%.3f;%.3f;%.3f;%.0f;%s,%s\n",
						song.getArtistLocation(), song.getArtistID(),
						song.getTempo(), song.getDanceability(),
						song.getEnergy(), song.getLoudness(),
						song.getDuration(), song.getTimeSignature(), arquivoTimbre);
				
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
				return "\n";
			}

		} catch (EchoNestException e) {

			e.printStackTrace();
		}
		return resultado;

	}

	// private int artistaEncontrado(List<Artist> artists, String
	// artistaDesejado) throws EchoNestException {
	// for (int i = 0; i< artists.size(); i++){
	// System.out.println(artists.get(i).getName());
	// if
	// (artists.get(i).getName().toLowerCase().contains(artistaDesejado.trim().toLowerCase())){
	// System.out.println("deu certo pra "+artistaDesejado);
	// return i;
	// }
	// }
	// System.out.println("foi procurar "+artistaDesejado+ " mas deu errado");
	// return -1;
	// }
	//
	// private int musicaEncontrada(List<Song> songs, String musicaDesejada)
	// throws EchoNestException {
	// for (int i = 0; i< songs.size(); i++){
	// if
	// (songs.get(i).getTitle().toLowerCase().contains(musicaDesejada.trim().toLowerCase())){
	// System.out.println(songs.get(i).getTitle().toLowerCase());
	// return i;
	// }
	// }
	// System.out.println("foi procurar "+musicaDesejada+
	// " mas deu errado em "+songs.size()+ " musicas");
	// return -1;
	// }
	
	private String transformaArrayString(double[] d){
		String result = "";
		for (int i = 0; i < d.length; i++){
			result+= d[i]+",";
		}
		return result.substring(0, result.length()-1);
	}

	private String getTimbres(Song song)  {
		String resultado = "";
		try {
			List<Segment> segmentos = song.getAnalysis().getSegments();
			for (Segment s : segmentos){
				resultado+=transformaArrayString(s.getTimbre())+"\n";
			}
		} catch (EchoNestException e) {
			System.out.println("Erro ao pegar segmentos");
			e.printStackTrace();
		}
		return resultado;
	}

	public static void main(String[] args) throws Exception {
		CriaBaseSongs sse = new CriaBaseSongs();
		String resultado = "";
		String naoEncontrados = "";
		String linha = "";
		String novosDados;
		int conta = 0;
		Path path = Paths.get("queries.txt");
		try {
			BufferedReader arquivo = Files.newBufferedReader(path, ENCODING);
			// BufferedReader arquivo = new BufferedReader(new
			// FileReader("queries.txt"));
			String[] arrayLinha;
			String linhaCrua;
			// 0 ano, 1 artista, 2 titulo, 3posicao
			while ((linhaCrua = arquivo.readLine()) != null) {
				System.out.println(++conta);
				sse.en.showStats();
				arrayLinha = linhaCrua.split(";");
				linha = String.format("%s;%s;%s;%s;", arrayLinha);
				try {
					novosDados = sse.searchSongByTitleAndArtist(arrayLinha[2],
							arrayLinha[1]);
				} catch (Exception e) {
					System.out.println("PEGOOOOOOOOOOOOOOU");
					sse.getProximaAPIKey();
					System.out.println("indiceatual é "+ sse.indexAtual);
					novosDados = sse.searchSongByTitleAndArtist(arrayLinha[2],
							arrayLinha[1]);
					System.out.println("novoDados ficou "+ novosDados);

				}

				if (novosDados.equals("\n")) {
					naoEncontrados += linha + "\n";
				} else {
					resultado += linha + novosDados;
				}

			}
			arquivo.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		FileWriter fstream = new FileWriter("songsComTimbreFaltantes.csv");
		BufferedWriter out = new BufferedWriter(fstream);
		out.write(resultado);
		out.close();
		fstream = new FileWriter("notFoundComTimbreFaltantes.csv");
		out = new BufferedWriter(fstream);
		out.write(naoEncontrados);
		out.close();

	}

}