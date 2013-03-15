package main;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import main.java.de.umass.lastfm.Tag;
import main.java.de.umass.lastfm.Track;

public class LastFmClient {

	private String APIKEY = null;
	private static String ENTRADA = null;// = "queries.txt";
	private static String SAIDA = null;// = "arquivo.txt";
	private static String CONTAGEM  = null;

	public LastFmClient(String APIKEY, String entrada, String saida, String contagem) {
		this.APIKEY = APIKEY;
		this.ENTRADA = entrada;
		this.SAIDA = saida;
		this.CONTAGEM = contagem;
	}

	public Collection<Tag> getTagsMusica(String artista, String musica) {
		return Track.getTopTags(artista, musica, APIKEY, "1");
	}

	public Collection<String> transformaTagToString(Collection<Tag> collection) {
		Collection<String> newCollection = new ArrayList<String>();

		for (Tag tag : collection) {
			newCollection.add(tag.getName() + ":"
					+ Integer.toString(tag.getCount()));
		}

		return newCollection;
	}

	public void escreveArquivo(String saida, String texto, boolean nsobrescrever)
			throws IOException {

		BufferedWriter arquivoSaida = new BufferedWriter(new FileWriter(saida,
				nsobrescrever));

		try {
			arquivoSaida.write(texto);
			arquivoSaida.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	public static void main(String[] args) {
		LastFmClient lfc = new LastFmClient("eeb6d4e4240082f4fbf2e51ce3fda8b9",args[0],args[1],args[2]);

		try {

			File file = new File(ENTRADA);
			FileReader fr = new FileReader(file);
			BufferedReader br = new BufferedReader(fr);
			File file2 = new File(CONTAGEM);
			FileReader fr2 = new FileReader(file2);
			BufferedReader br2 = new BufferedReader(fr2);
			String linha = "";
			String artista = "";
			String musica = "";
			String texto = "";
			int count = Integer.parseInt(br2.readLine());
			int marcador = 0;
			br2.close();
			while (marcador < count) {
				linha = br.readLine();
				marcador++;
			}
			while ((linha = br.readLine()) != null) {
				
				try {
					linha = linha.replace("\"", "");
					String[] splittedLine = linha.split("\t");
					artista = splittedLine[1];
					musica = splittedLine[2];

					Collection<Tag> art = lfc.getTagsMusica(artista, musica);
					Collection<String> artString = lfc
							.transformaTagToString(art);
					texto = linha + "\t" + artString.toString() + "\n";
					count++;
					System.out.println(count);
					lfc.escreveArquivo(CONTAGEM, Integer.toString(count),
							false);
					lfc.escreveArquivo(SAIDA, texto, true);
				} catch (Exception e) {
					System.out.println(e.getMessage());
				}
			}

		} catch (IOException e) {
			System.out.println(e.getMessage());
		}

	}
}