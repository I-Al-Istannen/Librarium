package de.ialistannen.librarium_dataentry

import android.content.Intent
import android.os.Bundle
import android.util.Log
import android.widget.*
import androidx.appcompat.app.AppCompatActivity
import com.ialistannen.librarium_dataentry.R
import de.ialistannen.librarium_dataentry.zxing.IntentIntegrator
import okhttp3.*
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.RequestBody.Companion.toRequestBody
import okhttp3.internal.EMPTY_REQUEST
import org.json.JSONArray
import java.io.IOException


class MainActivity : AppCompatActivity() {

    companion object {
        private const val TAG = "MainActivity"
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val spinner = findViewById<Spinner>(R.id.location_spinner)
        val scanButton = findViewById<ImageButton>(R.id.scan_isbn_button)
        val submitButton = findViewById<Button>(R.id.button_submit)
        val refreshLocationButton = findViewById<Button>(R.id.reloadLocationsButton)

        scanButton.setOnClickListener {
            IntentIntegrator(this).initiateScan()
        }

        val client = createClient()

        refreshLocationButton.setOnClickListener {
            updateLocations(client, spinner)
        }
        updateLocations(client, spinner)

        submitButton.setOnClickListener {
            addBook(spinner, client)
        }
    }

    private fun addBook(spinner: Spinner, client: OkHttpClient) {
        val isbn = findViewById<EditText>(R.id.editTextNumber).text.toString()
        val location = spinner.selectedItem as String

        val request = Request.Builder()
            .url("https://bib.ialistannen.de:666/book/${isbn}")
            .put(EMPTY_REQUEST)
            .build()

        client.newCall(request).enqueue(object : Callback {
            override fun onFailure(call: Call, e: IOException) {
                Log.w(TAG, "onFailure: request failed", e)
            }

            override fun onResponse(call: Call, response: Response) {
                if (!response.isSuccessful) {
                    runOnUiThread {
                        Toast.makeText(
                            this@MainActivity,
                            ":( ${response.message}",
                            Toast.LENGTH_SHORT
                        ).show()
                    }
                    return
                }

                setLocation(isbn, location)

                runOnUiThread {
                    Toast.makeText(
                        this@MainActivity,
                        "Success!",
                        Toast.LENGTH_SHORT
                    ).show()
                }
            }
        })
    }

    private fun setLocation(isbn: String, location: String) {
        val client = createClient()

        val request = Request.Builder()
            .url("https://bib.ialistannen.de:666/book/${isbn}/location")
            .put(location.toRequestBody("text/plain;charset=utf-8".toMediaType()))
            .build()

        client.newCall(request).enqueue(object : Callback {
            override fun onFailure(call: Call, e: IOException) {
                Log.w(TAG, "onFailure: location request failed", e)
            }

            override fun onResponse(call: Call, response: Response) {
                if (!response.isSuccessful) {
                    runOnUiThread {
                        Toast.makeText(
                            this@MainActivity,
                            ":( ${response.message}",
                            Toast.LENGTH_SHORT
                        ).show()
                    }
                    return
                }
                runOnUiThread {
                    Toast.makeText(
                        this@MainActivity,
                        "Set location!",
                        Toast.LENGTH_SHORT
                    ).show()
                }
            }
        })

    }

    private fun updateLocations(client: OkHttpClient, spinner: Spinner) {
        val request = Request.Builder()
            .url("https://bib.ialistannen.de:666/location")
            .get()
            .build()

        client.newCall(request).enqueue(object : Callback {
            override fun onFailure(call: Call, e: IOException) {
                Log.w(TAG, "onFailure: request failed", e)
            }

            override fun onResponse(call: Call, response: Response) {
                if (!response.isSuccessful) {
                    Toast.makeText(this@MainActivity, ":(", Toast.LENGTH_SHORT).show()
                    return
                }

                val locationsJson = JSONArray(response.body!!.string())
                val locations =
                    (0 until locationsJson.length())
                        .map { locationsJson[it].toString() }
                        .toTypedArray()

                runOnUiThread {
                    spinner.adapter = ArrayAdapter<String>(
                        this@MainActivity,
                        android.R.layout.simple_spinner_dropdown_item,
                        locations
                    )
                }
            }
        })
    }

    private fun createClient(): OkHttpClient {
        return OkHttpClient.Builder()
            .addInterceptor(Interceptor { chain ->
                val request = chain.request()
                val creds = Credentials.basic("Wachter", "Galaktische Freunde")

                chain.proceed(
                    request.newBuilder()
                        .addHeader("Authorization", creds)
                        .build()
                )
            })
            .build()
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        super.onActivityResult(requestCode, resultCode, data)

        val result = IntentIntegrator.parseActivityResult(requestCode, resultCode, data)
        findViewById<EditText>(R.id.editTextNumber).setText(result.contents)
    }
}